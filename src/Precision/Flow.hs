{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |
module Precision.Flow where

import Effectful
import Control.Monad.IO.Class
import Data.Yaml qualified as YAML
import System.Directory (getCurrentDirectory)
import System.FilePath  ((</>))

import GHC.Generics (Generic)

import OKA.Flow
import OKA.Flow.Eff
import OKA.Flow.Resources


-- | Dataflow monad specialized to OKA analises
type Flow' = Flow '[ProgConfigE]

runFlowHS :: ResultSet r => Flow' r -> IO ()
runFlowHS flow = do
  cfg <- readFlowConfig
  -- Resource for scheduler
  res <- pure
       =<< createResource (LockCoreCPU cfg.n_cores)
       =<< createResource (LockMemGB   cfg.memGB)
       =<< pure mempty
  -- Execution context
  ctx <- do
    cwd <- getCurrentDirectory
    pure FlowCtx
        { root      = cwd </> "store"
        , res       = res
        , logger    = mempty
        , runEffect = id
                    . runProgConfigE cfg.prog
                    . inject
        }
  runFlow ctx mempty flow


-- | Configuration for 
data FlowConfig = FlowConfig
  { n_cores :: !Int -- ^ Number of cores avala
  , memGB   :: !Int -- ^ Amount of memory
  , prog    :: ProgConfig
  }
  deriving stock    (Show,Generic)
  deriving anyclass (YAML.FromJSON)

readFlowConfig :: MonadIO m => m FlowConfig
readFlowConfig = liftIO $ YAML.decodeFileEither "config_flow.yaml" >>= \case
  Left e  -> error (show e)
  Right x -> pure x
