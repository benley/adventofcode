{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc924"
}:

# { sources ? import nix/sources.nix,
#   compiler ? "ghc924"
# }:

let
  # pkgs = import sources.nixpkgs {};

  haskellPackages = pkgs.haskell.packages.${compiler};

  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    # aeson
    # linear
    # colour
    # JuicyPixels
    # lens
    # yaml
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "aoc";
  buildInputs = [
    ghc
    pkgs.zlib
    haskellPackages.hoogle
    (pkgs.haskell-language-server.override { supportedGhcVersions = ["924"]; })
  ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
