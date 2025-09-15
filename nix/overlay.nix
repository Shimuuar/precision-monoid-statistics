self: super:
let
  dont_touch = [
    # Nix tools
    "buildHaskellPackages"
    "callCabal2nix"
    "callCabal2nixWithOptions"
    "callPackage"
    "ghcWithPackages"
    "haskellSrc2nix"
    "mkDerivation"
    "shellFor"
    # Executables
    "alex"
    "happy"
    # Libraries that come with GHC
    "Cabal"
    "Cabal-syntax"
    "array"
    "base"
    "binary"
    "bytestring"
    "containers"
    "deepseq"
    "directory"
    "exceptions"
    "filepath"
    "ghc"
    "ghc-bignum"
    "ghc-boot"
    "ghc-boot-th"
    "ghc-compact"
    "ghc-heap"
    "ghc-prim"
    "ghci"
    "haskeline"
    "hpc"
    "integer-gmp"
    "libiserv"
    "mtl"
    "parsec"
    "pretty"
    "process"
    "rts"
    "stm"
    "system-cxx-std-lib"
    "template-haskell"
    "terminfo"
    "text"
    "time"
    "transformers"
    "unix"
    "xhtml"
  ];
  banish = k: hs:
    if builtins.elem k dont_touch
    then hs
    else abort ("Package "+k+" is not in snapshot")
  ;
  # Local haskell packages
  lib           = self.haskell.lib;
  noProfile     = drv: (lib.disableExecutableProfiling (lib.enableLibraryProfiling drv));
  filterHaskell = builtins.filterSource (name: type:
    let base = builtins.baseNameOf name;
    in self.lib.cleanSourceFilter name type &&
       (type != "directory" || (base != "dist" && base != "dist-newstyle" && base != "TAGS"))
  );
  haskOverrides = hs_self: hs_super:
    let callHS = name: path: arg:
          lib.dontHaddock (noProfile (hs_self.callCabal2nix name (filterHaskell path) arg));
    in
      {
        precision = callHS "precision" ../hask/precision {python3-embed=self.python3;};
      };
  pyOverrides = self: super: {
    okaflow = self.callPackage (import ./python/okaflow.nix) {};
  };
in
{
  haskell98 = super.haskell.packages.ghc98.extend
    (hs_self: hs_super:
      (builtins.mapAttrs banish hs_super)
      //
      (import ./haskell/ghc98/nix/default.nix self hs_super)
      //
      (haskOverrides hs_self hs_super)
    );
  python312 = super.python312.override {
    packageOverrides = pyOverrides;
  };
}
