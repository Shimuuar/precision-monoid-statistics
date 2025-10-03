module Flow where

import Control.Monad
import OKA.Flow
import OKA.Flow.Std

import Precision.Flow
import Precision.Def



gogo :: Flow' ()
gogo = do
  appendMeta $ EvalMeanNaive
  appendMeta $ NSamples { size = 100
                        , samp = 10000
                        }
  appendMeta $ PRNG []
  xs <- forM [1 .. 10] $ \t -> scopeMeta $ do
    appendMeta $ Normal 0 t
    flowGenSample ()
  stdJupyter ["notebook/test.ipynb"] xs
