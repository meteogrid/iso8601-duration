{ mkDerivation, attoparsec, base, bytestring, hspec, hspec-core
, QuickCheck, stdenv
}:
mkDerivation {
  pname = "iso8601-duration";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ attoparsec base bytestring ];
  testHaskellDepends = [
    base bytestring hspec hspec-core QuickCheck
  ];
  homepage = "https://github.com/meteogrid/iso8601-duration";
  description = "Types and parser for ISO8601 durations";
  license = stdenv.lib.licenses.bsd3;
}
