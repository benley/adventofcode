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
    pkgs.python-language-server
    ghc
    pkgs.zlib
    haskellPackages.hoogle
    haskellPackages.haskell-language-server
    # (pkgs.haskell-language-server.override { supportedGhcVersions = ["92"]; })
  ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
