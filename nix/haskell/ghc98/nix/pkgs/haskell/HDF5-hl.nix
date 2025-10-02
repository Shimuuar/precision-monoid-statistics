{ mkDerivation, base, containers, deepseq, exceptions, fetchgit
, filepath, fixed-vector, HDF5-direct, lib, primitive, tasty
, tasty-hunit, tasty-quickcheck, temporary, text, transformers
, vector
}:
mkDerivation {
  pname = "HDF5-hl";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/Shimuuar/HFD5";
    sha256 = "1314hh6bvwl7sphwx6mmwl7hs01xc58v8fv227c1jyfv3m2c7p91";
    rev = "fa43f4091a38d9214ad1587a9242c6ac3e3ff02c";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/HDF5-hl; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base containers deepseq exceptions fixed-vector HDF5-direct
    primitive text transformers vector
  ];
  testHaskellDepends = [
    base filepath tasty tasty-hunit tasty-quickcheck temporary
  ];
  description = "High level API for HDF5 library";
  license = lib.licenses.bsd3;
}
