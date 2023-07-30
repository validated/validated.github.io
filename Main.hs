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
import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.Except (except, runExceptT)
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

data St = St
  { _stSync :: StSync,
    _stAsync :: StAsync
  }
  deriving stock (Eq, Ord, Generic)

data StSync = StSync
  { _stSyncMode :: Mode,
    _stSyncInfoTop :: [Info],
    _stSyncInfoMid :: [Info],
    _stSyncInfoLhs :: [Info],
    _stSyncInfoRhs :: [Info]
  }
  deriving stock (Eq, Ord, Show, Generic)

data StAsync = StAsync
  { _stAsyncSrc :: StSync,
    _stAsyncMid :: Async (Maybe Text),
    _stAsyncDst :: Maybe Text
  }
  deriving stock (Eq, Ord, Generic)

data Mode = View | Edit
  deriving stock (Eq, Ord, Show, Generic)

data Info = Info
  { _infoLabel :: Text,
    _infoValue :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)

instance A.ToJSON StSync

instance A.ToJSON Mode

instance A.ToJSON Info

instance A.FromJSON StSync

instance A.FromJSON Mode

instance A.FromJSON Info

makeLenses ''St
makeLenses ''StSync
makeLenses ''StAsync
makeLenses ''Info

defStSync :: StSync
defStSync =
  StSync
    { _stSyncMode = Edit,
      _stSyncInfoTop =
        [ Info "Logo" "static/tequila-shot.png",
          Info "Issuer" "Ministry of Tequila",
          Info "Title" "Tequila Consumption Certificate"
        ],
      _stSyncInfoMid =
        [ Info
            "Summary"
            "This individual has received all required Tequila shots \10004"
        ],
      _stSyncInfoLhs =
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
      _stSyncInfoRhs =
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

newStSync :: URI.URI -> Either String StSync
newStSync uri =
  case asumMap (qsGet qkCrt) $ URI.uriQuery uri of
    Nothing -> pure defStSync
    Just st0 -> do
      st1 <- B64URL.decode $ TE.encodeUtf8 st0
      A.eitherDecode $ BL.fromStrict st1

unStSync :: StSync -> Either TE.UnicodeException Text
unStSync =
  TE.decodeUtf8'
    . B64URL.encode
    . BL.toStrict
    . A.encode

newSt :: (MonadIO m) => URI.URI -> m (Either String St)
newSt uri =
  runExceptT $ do
    sst <- except $ newStSync uri
    aqr <-
      liftIO
        . async
        . pure
        . newQr
        . drvViewUri
        $ newDrv uri sst
    pure
      St
        { _stSync = sst,
          _stAsync =
            StAsync
              { _stAsyncSrc = sst,
                _stAsyncMid = aqr,
                _stAsyncDst = Nothing
              }
        }

data Drv = Drv
  { drvHomeUri :: Text,
    drvViewUri :: Text,
    drvEditUri :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)

xmrAddress :: Text
xmrAddress = "88XdfpP1mcNDykiA9xuv6afhgw3Arg5zNYayvVycJbS6UpdiZrTQHpQebFG9LmkKm6QYkRB68VVyiAq4FtBgYvu9LLUMPgn"

xmrWidget :: Maybe Text
xmrWidget =
  newQr xmrAddress

newDrv :: URI -> StSync -> Drv
newDrv uri sst =
  Drv
    { drvHomeUri =
        URI.render
          uri
            { URI.uriPath = Nothing,
              URI.uriQuery = mempty
            },
      drvViewUri = newUri View,
      drvEditUri = newUri Edit
    }
  where
    newUri x =
      URI.render
        . updateUri uri
        $ sst & stSyncMode .~ x

updateUri :: URI -> StSync -> URI
updateUri uri sst =
  --
  -- TODO : reasonable 404
  --
  fromRight uri $ do
    crt0 <- first SomeException $ unStSync sst
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
  res <- newSt uri
  case res of
    Left e -> fail e
    Right st ->
      startApp
        App
          { model = st,
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
  | SetField (SomeLens StSync Text) MisoString
  | DupField (SomeLens StSync [Info]) Int
  | DelField (SomeLens StSync [Info]) Int
  | SetAsync StAsync

updateModel ::
  URI ->
  Action ->
  St ->
  Effect Action St
updateModel _ Noop st = do
  st <# do
    let ast = st ^. stAsync
    res <- liftIO . poll $ st ^. stAsync . stAsyncMid
    case res of
      Just (Right dst)
        | ast ^. stAsyncSrc == st ^. stSync
            && ast ^. stAsyncDst /= dst ->
          pure . SetAsync $ ast & stAsyncDst .~ dst
      _ ->
        pure Noop
updateModel uri Init st = do
  st <# do
    let drv = newDrv uri $ st ^. stSync
    replaceUriAction drv st
    pure Noop
updateModel _ PrintPdf st =
  st <# do
    void $ JSaddle.eval @Text "window.print();"
    pure Noop
updateModel uri (SetField someLens val) st0 = do
  let st1 =
        st0
          & stSync . unSomeLens someLens .~ fromMisoString val
  st1 <# do
    let drv = newDrv uri $ st1 ^. stSync
    replaceUriAction drv st1
    ast <- newStAsync drv st1
    pure $ SetAsync ast
updateModel uri (DupField someLens idx0) st0 = do
  let st1 =
        st0
          & stSync . unSomeLens someLens
          %~ ( \xs -> do
                 (idx1, x) <- zip [0 ..] xs
                 if idx0 == idx1 then [x, x] else [x]
             )
  st1 <# do
    let drv = newDrv uri $ st1 ^. stSync
    replaceUriAction drv st1
    ast <- newStAsync drv st1
    pure $ SetAsync ast
updateModel uri (DelField someLens idx0) st0 = do
  let st1 =
        st0
          & stSync . unSomeLens someLens
          %~ ( \xs -> do
                 (idx1, x) <- zip [0 ..] xs
                 if idx0 == idx1 then [] else [x]
             )
  st1 <# do
    let drv = newDrv uri $ st1 ^. stSync
    replaceUriAction drv st1
    ast <- newStAsync drv st1
    pure $ SetAsync ast
updateModel uri (SetAsync ast) st0 = do
  let st1 =
        if st0 ^. stSync == ast ^. stAsyncSrc
          then st0 & stAsync .~ ast
          else st0
  st1 <# do
    let drv = newDrv uri $ st1 ^. stSync
    replaceUriAction drv st1
    pure Noop

replaceUriAction :: Drv -> St -> JSM ()
replaceUriAction drv st =
  whenJust
    ( parseURI
        . T.unpack
        $ case st ^. stSync . stSyncMode of
          View -> drvViewUri drv
          Edit -> drvEditUri drv
    )
    replaceURI

newStAsync :: (MonadIO m) => Drv -> St -> m StAsync
newStAsync drv st =
  liftIO $ do
    let src = st ^. stSync
    cancel $ st ^. stAsync . stAsyncMid
    mid <- async . pure . newQr $ drvViewUri drv
    pure
      StAsync
        { _stAsyncSrc = src,
          _stAsyncMid = mid,
          _stAsyncDst = Nothing
        }

--
-- View
--

viewApp :: URI -> St -> View Action
viewApp uri st =
  div_
    mempty
    [ link_ [rel_ "stylesheet", href_ "static/picnic.min.css"],
      link_ [rel_ "stylesheet", href_ "static/app.css"],
      case st ^. stSync . stSyncMode of
        Edit -> viewEditor drv st
        View -> viewModel st,
      script_ [src_ "static/clipboard.min.js"] mempty,
      script_ [src_ "static/patch.js", defer_ "defer"] mempty,
      script_ [src_ "static/app.js", defer_ "defer"] mempty
    ]
  where
    drv = newDrv uri $ st ^. stSync

copyClipboard :: Text -> View Action
copyClipboard txt =
  a_
    [ class_ "stack button text-center clipboardjs",
      textProp "data-clipboard-text" $ toMisoString txt
    ]
    [ kbd_ mempty [text "Copy \128203"]
    ]

viewLinks :: Drv -> [View Action]
viewLinks drv =
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
                    href_ $ drvHomeUri drv,
                    target_ "_blank"
                  ]
                  [ kbd_ mempty [text "Home \127968"]
                  ]
              ],
            div_
              [ class_ "full half-1100"
              ]
              [ a_
                  [ class_ "stack button text-center",
                    onClick PrintPdf
                  ]
                  [ kbd_ mempty [text "Save \128190"]
                  ]
              ],
            div_
              [ class_ "full half-1100"
              ]
              [ a_
                  [ class_ "stack button text-center",
                    href_ $ drvViewUri drv,
                    target_ "_blank"
                  ]
                  [ kbd_ mempty [text "View \128065"]
                  ]
              ],
            div_
              [ class_ "full half-1100"
              ]
              [ copyClipboard $
                  drvEditUri drv
              ],
            div_
              [ class_ "full half-1100"
              ]
              [ a_
                  [ class_ "stack button text-center",
                    href_ "https://github.com/validated/validated.github.io",
                    target_ "_blank"
                  ]
                  [ kbd_ mempty [text "Code \128187"]
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
                  [ kbd_ mempty [text "Info \128712"]
                  ]
              ]
          ]
      ]
  ]

viewEditor :: Drv -> St -> View Action
viewEditor drv st =
  div_
    [class_ "flex one four-700"]
    [ div_
        [class_ "one-fourth-700 no-print"]
        $ viewLinks drv
          <> [div_ [class_ "stack label"] [text "Editor"]]
          <> viewEditorSidebar (st ^. stSync)
          <> [div_ [class_ "stack-bottom"] mempty],
      div_
        [class_ "three-fourth-700 grow"]
        [ viewModel st,
          div_ [class_ "one no-print"] viewDonate
        ]
    ]

viewEditorSidebar :: StSync -> [View Action]
viewEditorSidebar sst =
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
    =<< ( infoIndex sst (SomeLens stSyncInfoTop)
            <> infoIndex sst (SomeLens stSyncInfoMid)
            <> infoIndex sst (SomeLens stSyncInfoLhs)
            <> infoIndex sst (SomeLens stSyncInfoRhs)
        )

infoIndex ::
  StSync ->
  SomeLens StSync [Info] ->
  [(Int, Info, SomeLens StSync [Info])]
infoIndex sst someLens =
  fmap (\(idx, val) -> (idx, val, someLens))
    . zip [0 :: Int ..]
    $ sst ^. (unSomeLens someLens)

viewDonate :: [View Action]
viewDonate =
  [ div_
      [class_ "stack label"]
      [text "Donate Monero (XMR)"],
    div_
      [ class_ "stack stack-middle"
      ]
      [ row_
          [div_ [class_ "qr long-text"] [text xmrAddress]]
      ],
    div_
      [ class_ "stack stack-middle"
      ]
      [ row_
          [div_ [class_ "qr"] [copyClipboard xmrAddress]]
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
             xmrWidget
       )

viewModel :: St -> View Action
viewModel st0 =
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
                  $ st1 ^. stSyncInfoTop
              ]
                <> [ half_
                       [ div_
                           [class_ "flex one center"]
                           $ maybe
                             [ div_
                                 [class_ "lds-dual-ring"]
                                 mempty
                             ]
                             ( \qr ->
                                 [img_ [class_ "qr", src_ qr]]
                             )
                             $ st0 ^. stAsync . stAsyncDst
                       ]
                   ]
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
            (st1 ^. stSyncInfoMid)
          <> [ row_
                 [ half_ . viewInfo $ st1 ^. stSyncInfoLhs,
                   half_ . viewInfo $ st1 ^. stSyncInfoRhs
                 ]
             ]
    ]
  where
    st1 = st0 ^. stSync

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
