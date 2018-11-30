{ mkDerivation, base, bifunctors, bytestring, clash-prelude
, cryptohash, data-default, deepseq, generic-arbitrary
, ghc-typelits-extra, ghc-typelits-knownnat
, ghc-typelits-natnormalise, hspec, lens, mtl, QuickCheck
, reflection, singletons, smallcheck, stdenv, tasty, tasty-hunit
, tasty-quickcheck, tasty-smallcheck, template-haskell
, type-level-sets
}:
mkDerivation {
  pname = "hash-cores";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bifunctors clash-prelude data-default deepseq
    generic-arbitrary ghc-typelits-extra ghc-typelits-knownnat
    ghc-typelits-natnormalise lens mtl QuickCheck reflection singletons
    template-haskell type-level-sets
  ];
  executableHaskellDepends = [
    base bytestring clash-prelude cryptohash deepseq ghc-typelits-extra
    ghc-typelits-knownnat ghc-typelits-natnormalise hspec lens
    QuickCheck smallcheck tasty tasty-hunit tasty-quickcheck
    tasty-smallcheck
  ];
  license = stdenv.lib.licenses.bsd3;
}
