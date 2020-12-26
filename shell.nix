{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    bazel
    haskell-language-server
    haskell.compiler.ghc8102
  ];
}
