{ mkDerivation, base, bifunctors, doctest, hspec
, hspec-expectations-lens, lens, QuickCheck, semigroupoids
, semigroups, stdenv
}:
mkDerivation {
  pname = "meep";
  version = "0.1.2.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors lens semigroupoids semigroups
  ];
  testHaskellDepends = [
    base bifunctors doctest hspec hspec-expectations-lens lens
    QuickCheck semigroupoids semigroups
  ];
  description = "A silly container";
  license = stdenv.lib.licenses.bsd3;
}
