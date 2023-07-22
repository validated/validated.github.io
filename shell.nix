with (import ./default.nix); let
  mkGhcid = mod:
    modern-pkgs.writeShellApplication {
      name = "ghcid-${mod}";
      text = ''(cd ${builtins.toString ./.} && ${modern-pkgs.ghcid}/bin/ghcid ${
          if mod == "exe"
          then ''--test=":run Main.main"''
          else ""
        } --command="${readmeGen}/bin/readmeGen && ${pkgs.haskell.packages.ghc865.cabal-install}/bin/cabal new-repl ${mod}:app --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j")'';
    };
in
  dev.env.overrideAttrs (old: {
    buildInputs =
      old.buildInputs
      ++ [
        modern-pkgs.hlint
        modern-pkgs.ormolu
        modern-pkgs.alejandra
        modern-pkgs.haskellPackages.cabal-fmt
        modern-pkgs.nodePackages.prettier
        modern-pkgs.zbar
        modern-pkgs.pandoc
        pkgs.haskell.packages.ghc865.cabal-install
        (mkGhcid "lib")
        (mkGhcid "exe")
        publish
      ];
  })
