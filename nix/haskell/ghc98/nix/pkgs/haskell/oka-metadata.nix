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
    sha256 = "156zvr4zr29l1qhg4s81ayhiv1r2jj180ycgdprma1wbwn3aa49p";
    rev = "aff305d57b5ee236744d83d50a60fef23a7deef6";
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
