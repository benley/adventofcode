{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    bazel
    bazel-buildtools
    haskell-language-server
    haskell.compiler.ghc8102
  ];
}
