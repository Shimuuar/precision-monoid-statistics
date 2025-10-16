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
    sha256 = "0xkgs53dzd7i02fcp5k0qfprbgq937ik91s5bdv96bxsa393lfc3";
    rev = "a8ec241aa18239baad85a8fae6a8aaa5e4929bd3";
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
