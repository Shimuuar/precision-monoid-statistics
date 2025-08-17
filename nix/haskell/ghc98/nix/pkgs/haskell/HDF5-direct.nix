{ mkDerivation, base, exceptions, fetchgit, hdf5, hdf5_hl, lib
, transformers
}:
mkDerivation {
  pname = "HDF5-direct";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/Shimuuar/HFD5";
    sha256 = "1aphijqdlrwk335abi7wxshdd094vns6aczf7qr97yk2xqs1j8kh";
    rev = "b3959dab33a7da7bb2292a23ad93d08089f4d988";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/HDF5-direct; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base exceptions transformers ];
  librarySystemDepends = [ hdf5 hdf5_hl ];
  description = "Low level warppers for HDF5 library";
  license = lib.licenses.bsd3;
}
