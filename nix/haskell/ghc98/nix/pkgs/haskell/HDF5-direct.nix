{ mkDerivation, base, exceptions, fetchgit, hdf5, hdf5_hl, lib
, transformers
}:
mkDerivation {
  pname = "HDF5-direct";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/Shimuuar/HFD5";
    sha256 = "1a9zwvyx9fnxb0s0nha8mvr6qrxnfscsk0my02z0n425znps4m3q";
    rev = "34d72fe59dda93bc09abbc272163d96f5a88c17d";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/HDF5-direct; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base exceptions transformers ];
  librarySystemDepends = [ hdf5 hdf5_hl ];
  description = "Low level warppers for HDF5 library";
  license = lib.licenses.bsd3;
}
