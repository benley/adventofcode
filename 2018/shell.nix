{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc881" }:

let
  inherit (nixpkgs) pkgs;
  haskellPackages =
    # pkgs.haskell.packages.${compiler};
    pkgs.haskellPackages;

  ghc = haskellPackages.ghcWithPackages (ps: with ps; [
    megaparsec
  ]);
in

pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
