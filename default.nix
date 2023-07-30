with (import (builtins.fetchTarball {
  url = "https://github.com/validated/miso/archive/fe007fa92211f7b058d329d81919a77511abb8f7.tar.gz";
  sha256 = "04yckh76cvrzw8yfbk49gqdmniw2qxqg5n592hxnnx8qdhwk0fd0";
}) {}); rec {
  dev = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. {
    miso = miso-jsaddle;
  };
  app = pkgs.haskell.packages.ghcjs.callCabal2nix "app" ./. {};
  repo = builtins.toString ./.;
  release = app.overrideAttrs (_: rec {
    postInstall = ''
      mkdir -p $out/bin/app.jsexe/static
      cp -R ${./static}/* $out/bin/app.jsexe/static
      cp ${./static}/favicon.ico $out/bin/app.jsexe/favicon.ico
      rm $out/bin/app.jsexe/static/readme.html || true
      cp ${readmeDer}/readme.html $out/bin/app.jsexe/static/
    '';
  });
  publish = modern-pkgs.writeShellApplication rec {
    name = "publish";
    text = let
      vsn = app.passthru.version;
    in ''
      (
        cd ${repo}
        nix-build -A publishDer
        mkdir -p ./docs/${vsn}
        cp -RL ./result/${vsn}/* ./docs/${vsn}
        cp -RLf ./result/${vsn}/favicon.ico ./docs/favicon.ico
        cp -RLf ./result/index.html ./docs/index.html
      )
    '';
  };
  publishDer = modern-pkgs.stdenv.mkDerivation {
    name = "publishDer";
    src = ./docs;
    dontBuild = true;
    installPhase = let
      vsn = app.passthru.version;
    in ''
      mkdir -p $out
      cp -R ./* $out || true
      if [ -d "$out/${vsn}" ]; then
        echo "Version ${vsn} does already exit!"
        exit 1
      else
        mkdir -p $out/${vsn}
        cp -R ${release}/bin/app.jsexe/* $out/${vsn}
        echo '<!doctype html><html><head><meta http-equiv="Refresh" content="0; url=${vsn}/index.html"></head><body></body></html>' > $out/index.html
        echo "Version ${vsn} has been published!"
      fi
    '';
  };
  readmeDer = modern-pkgs.stdenv.mkDerivation {
    name = "readmeDer";
    src = ./README.md;
    dontUnpack = true;
    buildPhase = ''
      ${modern-pkgs.pandoc}/bin/pandoc \
        --standalone \
        --metadata title="The Localists" \
        $src > readme.html
    '';
    installPhase = ''
      mkdir -p $out
      cp ./readme.html $out/readme.html
    '';
  };
  readmeGen = modern-pkgs.writeShellApplication rec {
    name = "readmeGen";
    text = ''
      ${modern-pkgs.pandoc}/bin/pandoc \
        --standalone \
        --metadata title="The Localists" \
        ${repo}/README.md > ${repo}/static/readme.html
    '';
  };
  modern-pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz";
    sha256 = "11w3wn2yjhaa5pv20gbfbirvjq6i3m7pqrq2msf0g7cv44vijwgw";
  }) {};
  inherit pkgs;
}
