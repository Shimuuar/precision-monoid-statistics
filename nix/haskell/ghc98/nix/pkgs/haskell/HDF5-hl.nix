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
    sha256 = "1a9zwvyx9fnxb0s0nha8mvr6qrxnfscsk0my02z0n425znps4m3q";
    rev = "34d72fe59dda93bc09abbc272163d96f5a88c17d";
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
