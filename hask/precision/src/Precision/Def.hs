{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes    #-}
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
import GHC.Generics (Generic)
import Python.Inline
import Python.Inline.QQ


import OKA.Flow
import OKA.Metadata.Encoding
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

data Estimator
  = EvalMeanNaive 
  | EvalMeanKBN
  deriving stock (Show,Read,Eq,Generic)
  deriving MetaEncoding via AsReadShow Estimator




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
