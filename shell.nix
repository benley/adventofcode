{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc924"
}:

let
  haskellPackages = pkgs.haskell.packages.${compiler};
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; []);
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