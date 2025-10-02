{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RoleAnnotations      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module Precision.Def where

import Control.Monad.ST
import Data.Aeson   (FromJSON,ToJSON)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
import Data.Word
import Data.Monoid.Statistics
import Data.Monoid.Statistics.Numeric
import System.Random.MWC
import System.Random.MWC.Distributions
import System.FilePath ((</>))
import GHC.Generics (Generic,Generically(..))
import Python.Inline
import Python.Inline.QQ

import HDF5.HL        qualified as H5
import HDF5.HL.Vector qualified as VH
import HDF5.HL.Serialize (H5Serialize(..))

import OKA.Flow
import OKA.Flow.Tools
import OKA.Metadata.Encoding
import OKA.Metadata
import Precision.Flow



----------------------------------------------------------------
--
----------------------------------------------------------------

data Distribution
  = Normal      !Double !Double
  | Exponential !Double
  -- | Cauchy
  deriving stock    (Show,Eq,Generic)
  deriving anyclass (FromJSON,ToJSON)
  deriving MetaEncoding via AsAeson Distribution
  deriving (IsMeta,IsMetaPrim) via AsMeta '["prec","distrib"] Distribution

data Estimator
  = EvalMeanNaive 
  | EvalMeanKBN
  deriving stock (Show,Read,Eq,Generic)
  deriving MetaEncoding via AsReadShow Estimator
  deriving (IsMeta,IsMetaPrim) via AsMeta '["prec","est"] Estimator

newtype NSamples = NSamples Int
  deriving stock (Show,Read,Eq,Generic)
  deriving newtype MetaEncoding
  deriving (IsMeta,IsMetaPrim) via AsMeta '["prec","N"] NSamples

newtype PRNG = PRNG [Word32]
  deriving stock (Show,Read,Eq,Generic)
  deriving newtype MetaEncoding
  deriving (IsMeta,IsMetaPrim) via AsMeta '["prec","PRNG"] PRNG


----------------------------------------------------------------
--
----------------------------------------------------------------

sample :: [Word32] -> Int -> Distribution -> VU.Vector Double
sample seed size d = VU.create $ do
  g <- restore $ toSeed $ VU.fromList seed
  let gen = case d of
        Normal      μ σ -> normal      μ σ g
        Exponential λ   -> exponential λ   g
  MVU.replicateM size gen

evalHaskell :: VU.Vector Double -> Estimator -> Double
evalHaskell xs = \case
  EvalMeanNaive -> partial $ calcMean (reduceSampleVec xs :: MeanNaive)
  EvalMeanKBN   -> partial $ calcMean (reduceSampleVec xs :: MeanKBN)

evalMpMath :: VU.Vector Double -> Estimator -> IO Double
evalMpMath xs = runPy . \case
  EvalMeanNaive -> pyMean
  EvalMeanKBN   -> pyMean
  where
    pyMean = fromPy' =<< [pye| sum(mp.mpf(x) for x in xs_hs) / len(xs_hs) |]

initMpmath :: IO ()
initMpmath = do
  initializePython
  runPy [pymain|
     import mpmath as mp
     mp.mp.dps = 60
     |]

data Res = Res
  { est   :: !Double
  , exact :: !Double
  }
  deriving stock (Show,Generic)
  deriving H5.Element via Generically Res

newtype Sample = Sample (VH.VecHDF5 Res)
  deriving stock (Show)
  deriving (FlowInput,FlowOutput,FlowArgument) via AsHDF5Encoded (VH.VecHDF5 Res)



flowGenSample
  :: () -> Flow' (Result Sample)
flowGenSample = liftHaskellFun "flow-gen-sample" () go where
  go :: (Distribution,Estimator,NSamples,PRNG) -> () -> IO Sample
  go (distrib,est,NSamples n,PRNG key) () = pure $ Sample $
    let xs = sample key n distrib
    in undefined
  

----------------------------------------------------------------
-- Flow definition
----------------------------------------------------------------



    
----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------
    
type role AsHDF5Encoded representational

-- | Encoding of outputs using HDF5 files
newtype AsHDF5Encoded a = AsHDF5Encoded a

instance (H5Serialize a) => FlowArgument (AsHDF5Encoded a) where
  type AsRes (AsHDF5Encoded a) = Result a
  parseFlowArguments = \case
    Param p -> Right $ readOutput p
    _       -> Left "Expecting single parameter"
  parameterShape x _ = x

instance (H5Serialize a) => FlowOutput (AsHDF5Encoded a) where
  writeOutput dir (AsHDF5Encoded a) = H5.withCreateFile (dir </> "data.h5") H5.CreateTrunc $ \h5 -> do
    h5Write h5 "dat" a

instance (H5Serialize a) => FlowInput (AsHDF5Encoded a) where
  readOutput dir = H5.withOpenFile (dir </> "data.h5") H5.OpenRO $ \h5 -> do
    AsHDF5Encoded <$> h5Read h5 "dat"
