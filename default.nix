{ mkDerivation, attoparsec, base, binary, bytestring, containers
, data-default, directory, filepath, stdenv, text, zlib
}:
mkDerivation {
  pname = "dictionaries";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base binary bytestring containers data-default directory
    filepath text zlib
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
