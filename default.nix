{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, checkers, containers
      , directory, hedis, hpack, hspec, hspec-discover
      , integer-logarithms, lib, network, network-uri
      , optparse-applicative, QuickCheck, random, raw-strings-qq, scotty
      , text, time, transformers, trifecta, uuid, vector
      }:
      mkDerivation {
        pname = "haskellbook";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring checkers containers directory hedis hspec
          integer-logarithms network network-uri optparse-applicative
          QuickCheck random raw-strings-qq scotty text time transformers
          trifecta uuid vector
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [ base hspec hspec-discover QuickCheck ];
        testToolDepends = [ hspec-discover ];
        prePatch = "hpack";
        homepage = "https://github.com/evenbrenden/haskellbook#readme";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
