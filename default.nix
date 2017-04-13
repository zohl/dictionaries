{ mkDerivation, attoparsec, base, binary, bytestring, containers
, criterion, data-default, deepseq, directory, exceptions, filepath
, hspec, QuickCheck, random, stdenv, text, time, transformers, zlib
}:
mkDerivation {
  pname = "dictionaries";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base binary bytestring containers data-default deepseq
    directory exceptions filepath text time transformers zlib
  ];
  executableHaskellDepends = [
    base bytestring containers criterion deepseq directory filepath
    random text
  ];
  testHaskellDepends = [
    base bytestring containers directory filepath hspec QuickCheck
    random text time
  ];
  description = "Tools to handle StarDict dictionaries";
  license = stdenv.lib.licenses.bsd3;
}
