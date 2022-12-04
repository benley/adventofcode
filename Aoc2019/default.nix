let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/f8248ab6d9e69ea9c07950d73d48807ec595e923";
    sha256 = "009i9j6mbq6i481088jllblgdnci105b2q4mscprdawg3knlyahk";
  };

  all-hies = fetchTarball {
    url = "https://github.com/infinisil/all-hies/tarball/534ac517b386821b787d1edbd855b9966d0c0775";
    sha256 = "0bw1llpwxbh1dnrnbxkj2l0j58s523hjivszf827c3az5i4py1i2";
  };

  pkgs = import nixpkgs {
    config = {};
    overlays = [
      (import all-hies {}).overlay
    ];
  };
  inherit (pkgs) lib;

  set = pkgs.haskell.packages.ghc865.override (old: {
    overrides = lib.composeExtensions old.overrides (hself: hsuper: {
      aoc2019 = hself.callCabal2nix "aoc2019" (lib.sourceByRegex ./. [
        "^.*\\.hs$"
        "^.*\\.cabal$"
      ]) {
        # Needs to match cabal-install version
        # Cabal = hself.Cabal_3_0_0_0;
      };
    });
  });

in pkgs.haskell.lib.justStaticExecutables set.aoc2019 // {
  env = set.shellFor {
    packages = p: [ p.aoc2019 ];
    nativeBuildInputs = [
      set.cabal-install
      set.hie
    ];
    withHoogle = true;
    shellHook = ''
      export HIE_HOOGLE_DATABASE=$(realpath "$(dirname "$(realpath "$(which hoogle)")")/../share/doc/hoogle/default.hoo")
    '';
  };
  inherit set;
}
