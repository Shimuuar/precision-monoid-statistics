{ mkDerivation, aeson, async, base, base16-bytestring, bytestring
, containers, cryptohash-sha1, directory, effectful-core, fetchgit
, filepath, lens, lib, mtl, oka-metadata, optparse-applicative
, process, random, stm, tasty, tasty-hunit, tasty-quickcheck
, temporary, text, time, transformers, typed-process, unix
, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "oka-flow";
  version = "0.1";
  src = fetchgit {
    url = "http://github.com/Shimuuar/oka-flow.git";
    sha256 = "156zvr4zr29l1qhg4s81ayhiv1r2jj180ycgdprma1wbwn3aa49p";
    rev = "aff305d57b5ee236744d83d50a60fef23a7deef6";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/oka-flow; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base base16-bytestring bytestring containers
    cryptohash-sha1 directory effectful-core filepath lens mtl
    oka-metadata process random stm temporary text time transformers
    typed-process unix unordered-containers vector yaml
  ];
  executableHaskellDepends = [
    aeson base containers directory filepath optparse-applicative
    transformers
  ];
  testHaskellDepends = [
    aeson base containers directory effectful-core filepath lens
    oka-metadata tasty tasty-hunit tasty-quickcheck temporary text
    transformers unordered-containers
  ];
  description = "Simple dataflow programming framework";
  license = lib.licenses.bsd3;
  mainProgram = "oka-flow";
}
