{ mkDerivation, attoparsec, base, bytestring, bytestring-lexing
, hspec, hspec-core, QuickCheck, quickcheck-instances, stdenv, time
}:
mkDerivation {
  pname = "iso8601-duration";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring bytestring-lexing time
  ];
  testHaskellDepends = [
    base bytestring hspec hspec-core QuickCheck quickcheck-instances
  ];
  homepage = "https://github.com/meteogrid/iso8601-duration";
  description = "Types and parser for ISO8601 durations";
  license = stdenv.lib.licenses.bsd3;
}
