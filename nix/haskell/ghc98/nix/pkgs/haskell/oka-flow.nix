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
    sha256 = "0xkgs53dzd7i02fcp5k0qfprbgq937ik91s5bdv96bxsa393lfc3";
    rev = "a8ec241aa18239baad85a8fae6a8aaa5e4929bd3";
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
