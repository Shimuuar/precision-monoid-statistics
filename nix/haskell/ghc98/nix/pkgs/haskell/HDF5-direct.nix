{ mkDerivation, base, exceptions, fetchgit, hdf5, hdf5_hl, lib
, transformers
}:
mkDerivation {
  pname = "HDF5-direct";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/Shimuuar/HFD5";
    sha256 = "1314hh6bvwl7sphwx6mmwl7hs01xc58v8fv227c1jyfv3m2c7p91";
    rev = "fa43f4091a38d9214ad1587a9242c6ac3e3ff02c";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/HDF5-direct; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base exceptions transformers ];
  librarySystemDepends = [ hdf5 hdf5_hl ];
  description = "Low level warppers for HDF5 library";
  license = lib.licenses.bsd3;
}
