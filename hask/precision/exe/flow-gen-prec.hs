module Main where

import Precision.Def

import OKA.Flow.Tools

main :: IO ()
main = do
  meta   <- metaFromStdin
  sample <- generateSample meta ()
  writeOutput "." sample
