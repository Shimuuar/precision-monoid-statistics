{ mkDerivation, base, deepseq, exceptions, fetchgit, filepath
, fixed-vector, HDF5-direct, lib, primitive, tasty, tasty-hunit
, tasty-quickcheck, temporary, transformers, vector
}:
mkDerivation {
  pname = "HDF5-hl";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/Shimuuar/HFD5";
    sha256 = "1aphijqdlrwk335abi7wxshdd094vns6aczf7qr97yk2xqs1j8kh";
    rev = "b3959dab33a7da7bb2292a23ad93d08089f4d988";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/HDF5-hl; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base deepseq exceptions fixed-vector HDF5-direct primitive
    transformers vector
  ];
  testHaskellDepends = [
    base filepath tasty tasty-hunit tasty-quickcheck temporary
  ];
  description = "High level API for HDF5 library";
  license = lib.licenses.bsd3;
}
