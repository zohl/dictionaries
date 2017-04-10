{ mkDerivation, attoparsec, base, binary, bytestring, containers
, data-default, directory, exceptions, filepath, hspec, QuickCheck
, random, stdenv, text, time, transformers, zlib
}:
mkDerivation {
  pname = "dictionaries";
  version = "0.1.0.1";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base binary bytestring containers data-default directory
    exceptions filepath text time transformers zlib
  ];
  testHaskellDepends = [
    base bytestring containers directory filepath hspec QuickCheck
    random text time
  ];
  description = "Tools to handle StarDict dictionaries";
  license = stdenv.lib.licenses.bsd3;
}
