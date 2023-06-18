{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
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
    haskellPackages.haskell-language-server
    pkgs.niv
  ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
