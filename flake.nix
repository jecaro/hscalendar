{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hlib = pkgs.haskell.lib;
      haskellPackages = pkgs.haskellPackages.override {
        overrides = hfinal: hprev: {
          haskell-to-elm = hlib.doJailbreak (hlib.unmarkBroken hprev.haskell-to-elm);
        };
      };

      # The backend in Haskell
      backend = haskellPackages.callCabal2nix "hscalendar" ./. { };

      # The Elm frontend.
      # Elm dependencies are pinned with elm2nix (elm-srcs.nix + registry.dat);
      # regenerate them with:
      #   cd frontend && elm2nix convert > elm-srcs.nix && elm2nix snapshot
      frontend = pkgs.stdenv.mkDerivation {
        pname = "hscalendar-frontend";
        version = "0.1.0.0";
        src = ./frontend;
        nativeBuildInputs = [ pkgs.elmPackages.elm ];
        buildPhase =
          (pkgs.elmPackages.fetchElmDeps {
            elmPackages = import ./frontend/elm-srcs.nix;
            elmVersion = "0.19.1";
            registryDat = ./frontend/registry.dat;
          })
          + ''
            ${backend}/bin/elm-generator -o src
            elm make src/Main.elm --optimize --output=elm.js
          '';
        installPhase = ''
          mkdir -p $out
          cp -r elm.js index.html assets $out/
        '';
      };

      hscalendar = pkgs.runCommand "hscalendar-0.1.0.0" { } ''
        mkdir -p $out
        ln -s ${backend}/bin/* $out/
        ln -s ${frontend} $out/frontend
      '';
    in
    {
      packages.x86_64-linux = {
        inherit backend frontend hscalendar;
        default = hscalendar;
      };

      devShells.x86_64-linux.default =
        haskellPackages.shellFor {
          packages = p: [ backend ];
          withHoogle = true;
          buildInputs = [
            haskellPackages.cabal-install
            haskellPackages.ghcid
            haskellPackages.haskell-language-server
            pkgs.elm2nix
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
          ];
        };
    };
}
