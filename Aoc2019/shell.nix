{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, containers, fgl, split, stdenv
      , text, text-show, text-format
      }:
      mkDerivation {
        pname = "aoc2019";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          array base containers fgl split
          text text-show text-format
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
