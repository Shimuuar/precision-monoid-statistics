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
    sha256 = "11hsw6qnkqsk05j7dgamjahl4jg0iwjn5pwxqs1mmq0v0z8cl0d8";
    rev = "8ad7b2756426b2db569ec520775a04b6a52a6cd7";
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
