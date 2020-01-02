{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, async, containers, dlist, fgl, split, stdenv
      , string-qq, text, transformers
      }:
      mkDerivation {
        pname = "aoc2019";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base async containers dlist fgl split string-qq text transformers
        ];
        doHaddock = false;
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
