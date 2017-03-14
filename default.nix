{ mkDerivation, attoparsec, base, binary, bytestring, containers
, data-default, directory, exceptions, filepath, stdenv, text, time
, zlib
}:
mkDerivation {
  pname = "dictionaries";
  version = "0.1.0.1";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base binary bytestring containers data-default directory
    exceptions filepath text time zlib
  ];
  description = "Tools to handle StarDict dictionaries";
  license = stdenv.lib.licenses.bsd3;
}
