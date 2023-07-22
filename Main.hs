{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Ws
#endif
import BasePrelude hiding (app)
import qualified Codec.QRCode as QR
  ( ErrorLevel (..),
    TextEncoding (Iso8859_1OrUtf8WithoutECI),
    defaultQRCodeOptions,
    encodeAutomatic,
  )
import Control.Monad.Extra (whenJust)
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy as TL
import qualified Language.Javascript.JSaddle.Evaluate as JSaddle
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Miso hiding
  ( Text,
    URI,
    class_,
    href_,
    src_,
    text,
    uriQuery,
    value_,
  )
import qualified Miso
import Miso.String (MisoString, fromMisoString, toMisoString)
import Network.URI (parseURI)
import qualified QR
import Text.URI (URI)
import qualified Text.URI as URI
import qualified Text.URI.QQ as URI hiding (uri)

--
-- Model
--

data Model = Model
  { _modMode :: Mode,
    _modInfoTop :: [Info],
    _modInfoMid :: [Info],
    _modInfoLhs :: [Info],
    _modInfoRhs :: [Info]
  }
  deriving stock (Eq, Ord, Show, Generic)

data Mode = View | Edit
  deriving stock (Eq, Ord, Show, Generic)

data Info = Info
  { _infoLabel :: Text,
    _infoValue :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)

instance A.ToJSON Model

instance A.ToJSON Mode

instance A.ToJSON Info

instance A.FromJSON Model

instance A.FromJSON Mode

instance A.FromJSON Info

makeLenses ''Model
makeLenses ''Info

defModel :: Model
defModel =
  Model
    { _modMode = Edit,
      _modInfoTop =
        [ Info "Logo" "static/tequila-shot.png",
          Info "Issuer" "Ministry of Tequila",
          Info "Title" "Tequila Consumption Certificate"
        ],
      _modInfoMid =
        [ Info
            "Summary"
            "This individual has received all required Tequila shots \10004"
        ],
      _modInfoLhs =
        [ Info
            "Surname(s) and forename(s)"
            "John Dough",
          Info
            "Date of birth"
            "1985-02-12",
          Info
            "Gender"
            "Male",
          Info
            "Passport or other identification document number"
            "DN1234567"
        ],
      _modInfoRhs =
        [ Info
            "Plant or crop targeted"
            "Blue Agave",
          Info
            "Tequila product"
            "Reserva",
          Info
            "Tequila type"
            "Golden",
          Info
            "Tequila manufacturer"
            "Tequila-AlcoNTech",
          Info
            "Primary Tequila shots"
            "2/2",
          Info
            "Booster Tequila shots"
            "3/3",
          Info
            "Date of the latest Tequila shot received"
            "2021-06-23",
          Info
            "Country of Tequila consumption"
            "Tequiland"
        ]
    }

newModel :: URI.URI -> Either String Model
newModel uri =
  case asumMap (qsGet qkCrt) $ URI.uriQuery uri of
    Nothing -> pure defModel
    Just st0 -> do
      st1 <- B64URL.decode $ TE.encodeUtf8 st0
      A.eitherDecode $ BL.fromStrict st1

unModel :: Model -> Either TE.UnicodeException Text
unModel =
  TE.decodeUtf8'
    . B64URL.encode
    . BL.toStrict
    . A.encode

data Derived = Derived
  { derHomeUri :: Text,
    derViewUri :: Text,
    derEditUri :: Text,
    derAddrXmr :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)

newDerived :: URI -> Model -> Derived
newDerived uri st =
  Derived
    { derHomeUri =
        URI.render
          uri
            { URI.uriPath = Nothing,
              URI.uriQuery = mempty
            },
      derViewUri = newUri View,
      derEditUri = newUri Edit,
      derAddrXmr = "88XdfpP1mcNDykiA9xuv6afhgw3Arg5zNYayvVycJbS6UpdiZrTQHpQebFG9LmkKm6QYkRB68VVyiAq4FtBgYvu9LLUMPgn"
    }
  where
    newUri x = URI.render . updateUri uri $ st & modMode .~ x

updateUri :: URI -> Model -> URI
updateUri uri x =
  --
  -- TODO : reasonable 404
  --
  fromRight uri $ do
    crt0 <- first SomeException $ unModel x
    crt1 <- URI.mkQueryValue crt0
    pure $
      uri
        { URI.uriQuery = [URI.QueryParam qkCrt crt1]
        }

--
-- Main
--

main :: IO ()
main = runApp $ do
  uri <- URI.mkURI . T.pack . show =<< getCurrentURI
  case newModel uri of
    Left e -> fail e
    Right x -> do
      startApp
        App
          { model = x,
            update = updateModel uri,
            view = viewApp uri,
            subs = mempty,
            events = defaultEvents,
            initialAction = Init,
            mountPoint = Nothing,
            logLevel = Off
          }

--
-- Pain
--

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp app =
  Warp.runSettings
    ( Warp.setPort
        8080
        (Warp.setTimeout 3600 Warp.defaultSettings)
    )
    =<< JSaddle.jsaddleOr
      Ws.defaultConnectionOptions
      (app >> syncPoint)
      router
  where
    router req sendResp =
      case Wai.pathInfo req of
        ("static" : _) ->
          staticApp (defaultWebAppSettings ".") req sendResp
        --
        -- TODO : 404
        --
        _ ->
          JSaddle.jsaddleApp req sendResp
#else
runApp :: IO () -> IO ()
runApp = id
#endif

src_ :: Text -> Attribute action
src_ =
  Miso.src_ . toMisoString

href_ :: Text -> Attribute action
href_ =
  Miso.href_ . toMisoString

class_ :: Text -> Attribute action
class_ =
  Miso.class_ . toMisoString

text :: Text -> View action
text =
  Miso.text . toMisoString

--
-- Util
--

asumMap ::
  forall b m f a.
  ( Foldable f,
    Monoid (m b)
  ) =>
  (a -> m b) ->
  f a ->
  m b
asumMap f =
  foldr (mappend . f) mempty

newQr :: Text -> Maybe Text
newQr =
  (TL.toStrict . QR.toBmpDataUrlT 0 5 <$>)
    . QR.encodeAutomatic
      (QR.defaultQRCodeOptions QR.L)
      QR.Iso8859_1OrUtf8WithoutECI

newtype SomeLens s a = SomeLens
  { unSomeLens :: Traversal' s a
  }

row_ :: [View action] -> View action
row_ =
  div_ [class_ "flex one center"]

row__ :: Text -> [View action] -> View action
row__ x =
  div_ [class_ $ "flex one center " <> x]

half_ :: [View action] -> View action
half_ =
  div_ [class_ "full half-500"]

--
-- URI
--

qkCrt :: URI.RText 'URI.QueryKey
qkCrt = [URI.queryKey|c|]

qsGet ::
  URI.RText 'URI.QueryKey ->
  URI.QueryParam ->
  Maybe Text
qsGet qk = \case
  URI.QueryParam k x | k == qk -> Just $ URI.unRText x
  _ -> Nothing

--
-- Controller
--

data Action
  = Noop
  | Init
  | PrintPdf
  | SetField (SomeLens Model Text) MisoString
  | DupField (SomeLens Model [Info]) Int
  | DelField (SomeLens Model [Info]) Int

updateModel :: URI -> Action -> Model -> Effect Action Model
updateModel _ Noop st =
  noEff st
updateModel uri Init st = do
  st <# do
    replaceUriAction uri st
    pure Noop
updateModel _ PrintPdf st =
  st <# do
    void $ JSaddle.eval @Text "window.print();"
    pure Noop
updateModel uri (SetField someLens val) st0 = do
  let st1 =
        st0
          & unSomeLens someLens
          .~ fromMisoString val
  st1 <# do
    replaceUriAction uri st1
    pure Noop
updateModel uri (DupField someLens idx0) st0 = do
  let st1 =
        st0
          & unSomeLens someLens
          %~ ( \xs -> do
                 (idx1, x) <- zip [0 ..] xs
                 if idx0 == idx1 then [x, x] else [x]
             )
  st1 <# do
    replaceUriAction uri st1
    pure Noop
updateModel uri (DelField someLens idx0) st0 = do
  let st1 =
        st0
          & unSomeLens someLens
          %~ ( \xs -> do
                 (idx1, x) <- zip [0 ..] xs
                 if idx0 == idx1 then [] else [x]
             )
  st1 <# do
    replaceUriAction uri st1
    pure Noop

replaceUriAction :: URI -> Model -> JSM ()
replaceUriAction uri st =
  whenJust
    ( parseURI
        . T.unpack
        . derEditUri
        $ newDerived uri st
    )
    replaceURI

--
-- View
--

viewApp :: URI -> Model -> View Action
viewApp uri x =
  div_
    mempty
    [ link_ [rel_ "stylesheet", href_ "static/picnic.min.css"],
      link_ [rel_ "stylesheet", href_ "static/app.css"],
      case x ^. modMode of
        Edit -> viewEditor der x
        View -> viewModel der x,
      script_ [src_ "static/clipboard.min.js"] mempty,
      script_ [src_ "static/patch.js", defer_ "defer"] mempty,
      script_ [src_ "static/app.js", defer_ "defer"] mempty
    ]
  where
    der = newDerived uri x

copyClipboard :: Text -> View Action
copyClipboard txt =
  a_
    [ class_ "stack button text-center clipboardjs",
      textProp "data-clipboard-text" $ toMisoString txt
    ]
    [ text "Copy \128203"
    ]

viewLinks :: Derived -> [View Action]
viewLinks der =
  [ div_
      [class_ "stack label"]
      [text "Links"],
    div_
      [ class_ "stack one no-offset-bottom-right stack-middle"
      ]
      [ div_
          [ class_ "flex flex-justify"
          ]
          [ div_
              [ class_ "full half-1100"
              ]
              [ a_
                  [ class_ "stack button text-center",
                    href_ $ derHomeUri der,
                    target_ "_blank"
                  ]
                  [ text "Home \127968"
                  ]
              ],
            div_
              [ class_ "full half-1100"
              ]
              [ a_
                  [ class_ "stack button text-center",
                    onClick PrintPdf
                  ]
                  [ text "Save \128190"
                  ]
              ],
            div_
              [ class_ "full half-1100"
              ]
              [ a_
                  [ class_ "stack button text-center",
                    href_ $ derViewUri der,
                    target_ "_blank"
                  ]
                  [ text "View \128065"
                  ]
              ],
            div_
              [ class_ "full half-1100"
              ]
              [ copyClipboard $
                  derEditUri der
              ],
            div_
              [ class_ "full half-1100"
              ]
              [ a_
                  [ class_ "stack button text-center",
                    href_ "https://github.com/validated/validated.github.io",
                    target_ "_blank"
                  ]
                  [ text "Code \128187"
                  ]
              ],
            div_
              [ class_ "full half-1100"
              ]
              [ a_
                  [ class_ "stack button text-center",
                    href_ "static/readme.html",
                    target_ "_blank"
                  ]
                  [ text "Info \128712"
                  ]
              ]
          ]
      ]
  ]

viewEditor :: Derived -> Model -> View Action
viewEditor der x =
  div_
    [class_ "flex one four-700"]
    [ div_
        [class_ "one-fourth-700 no-print"]
        $ viewLinks der
          <> [div_ [class_ "stack label"] [text "Editor"]]
          <> viewEditorSidebar x
          <> [div_ [class_ "stack-bottom"] mempty],
      div_
        [class_ "three-fourth-700 grow"]
        [ viewModel der x,
          div_ [class_ "one no-print"] $
            viewDonate der
        ]
    ]

viewEditorSidebar :: Model -> [View Action]
viewEditorSidebar x =
  ( \(idx, info, someLens) ->
      [ textarea_
          [ class_ "stack",
            onInput
              ( SetField $
                  SomeLens $
                    unSomeLens someLens . ix idx . infoLabel
              )
          ]
          [text $ info ^. infoLabel],
        textarea_
          ( [ class_ "stack stack-middle",
              onInput
                ( SetField $
                    SomeLens $
                      unSomeLens someLens . ix idx . infoValue
                )
            ]
          )
          [text $ info ^. infoValue],
        div_
          [ class_
              "stack stack-middle four flex-justify border-top"
          ]
          [ button_
              [ class_ "button one-fourth",
                onClick $ DupField someLens idx
              ]
              [text "+"],
            button_
              [ class_ "button one-fourth",
                onClick $ DelField someLens idx
              ]
              [text "-"]
          ]
      ]
  )
    =<< ( infoIndex x (SomeLens modInfoTop)
            <> infoIndex x (SomeLens modInfoMid)
            <> infoIndex x (SomeLens modInfoLhs)
            <> infoIndex x (SomeLens modInfoRhs)
        )

infoIndex ::
  Model ->
  SomeLens Model [Info] ->
  [(Int, Info, SomeLens Model [Info])]
infoIndex x someLens =
  fmap (\(idx, val) -> (idx, val, someLens))
    . zip [0 :: Int ..]
    $ x ^. (unSomeLens someLens)

viewDonate :: Derived -> [View Action]
viewDonate der =
  [ div_
      [class_ "stack label"]
      [text "Donate Monero (XMR)"],
    div_
      [ class_ "stack stack-middle"
      ]
      [ row_
          [div_ [class_ "qr long-text"] [text $ derAddrXmr der]]
      ],
    div_
      [ class_ "stack stack-middle"
      ]
      [ row_
          [div_ [class_ "qr"] [copyClipboard $ derAddrXmr der]]
      ]
  ]
    <> ( maybeToList $
           fmap
             ( \qr ->
                 div_
                   [class_ "stack stack-bottom"]
                   [ row__ "align-center" $
                       [img_ [class_ "qr", src_ qr]]
                   ]
             )
             (newQr $ derAddrXmr der)
       )

viewModel :: Derived -> Model -> View Action
viewModel der x =
  div_
    [ class_ "flex one center"
    ]
    [ div_
        [ class_
            . T.intercalate " "
            $ [ "full",
                "full-500",
                "full-600",
                "full-700",
                "four-fifth-800",
                "four-fifth-900",
                "four-fifth-1000",
                "three-fourth-1100",
                "three-fourth-1200",
                "three-fourth-1300",
                "two-third-1400",
                "two-third-1500",
                "two-third-1600",
                "three-fifth-1700",
                "three-fifth-1800",
                "half-1900",
                "two-fifth-2000"
              ]
        ]
        $ [ row__ "align-center" $
              [ half_
                  . fmap
                    ( \info ->
                        let txt = info ^. infoValue
                         in if isRight $ URI.mkURI txt
                              then
                                row_
                                  [ img_
                                      [ class_ "logo",
                                        src_ txt
                                      ]
                                  ]
                              else
                                h3_
                                  [class_ "text-center"]
                                  [text txt]
                    )
                  $ x ^. modInfoTop
              ]
                <> ( maybeToList
                       . fmap
                         ( \qr ->
                             half_
                               [ div_
                                   [class_ "flex one center"]
                                   [img_ [class_ "qr", src_ qr]]
                               ]
                         )
                       . newQr
                       $ derViewUri der
                   )
          ]
          <> fmap
            ( \info ->
                article_
                  [class_ "card summary text-center"]
                  [ header_
                      mempty
                      [ h4_
                          mempty
                          [text $ info ^. infoValue]
                      ]
                  ]
            )
            (x ^. modInfoMid)
          <> [ row_
                 [ half_ . viewInfo $ x ^. modInfoLhs,
                   half_ . viewInfo $ x ^. modInfoRhs
                 ]
             ]
    ]

viewInfo :: [Info] -> [View Action]
viewInfo =
  fmap $ \x ->
    viewNamedRow (x ^. infoLabel)
      . text
      $ x ^. infoValue

viewNamedRow :: Text -> View Action -> View Action
viewNamedRow name widget =
  row_
    [ half_ [text name],
      half_ [b_ mempty [widget]]
    ]
