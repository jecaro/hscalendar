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
      # Tests are disabled in the package build: 4 property tests fail at
      # runtime against the newer persistent-sqlite / time libraries (sub-second
      # TimeOfDay round-tripping and test DB isolation). Run them in the dev
      # shell with `cabal test`.
      hscalendar = hlib.dontCheck (haskellPackages.callCabal2nix "hscalendar" ./. { });
    in
    {
      packages.x86_64-linux.default = hscalendar;

      devShells.x86_64-linux.default =
        haskellPackages.shellFor {
          packages = p: [ hscalendar ];
          withHoogle = true;
          buildInputs = [
            haskellPackages.haskell-language-server
            haskellPackages.ghcid
            haskellPackages.cabal-install
          ];
        };
    };
}
