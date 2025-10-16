{ mkDerivation, aeson, attoparsec, base, containers, fetchgit
, fixed-vector, generic-arbitrary, lens, lib, quickcheck-instances
, scientific, tasty, tasty-hunit, tasty-quickcheck, text, these
, transformers, vector, yaml
}:
mkDerivation {
  pname = "oka-metadata";
  version = "0.1";
  src = fetchgit {
    url = "http://github.com/Shimuuar/oka-flow.git";
    sha256 = "0q7qjrvr42bs2d7y3lx4wsk1v0qjsaya7vmgbi7h0vz43y29fn2i";
    rev = "a39d29c5cead7f01d358c9eba2b1c05426601287";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/oka-metadata; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson attoparsec base containers fixed-vector lens scientific text
    these transformers vector yaml
  ];
  testHaskellDepends = [
    aeson base containers fixed-vector generic-arbitrary
    quickcheck-instances tasty tasty-hunit tasty-quickcheck text vector
  ];
  description = "Metadata for data analysis";
  license = lib.licenses.bsd3;
}
