module Main where

import           Manifold    (numberOfSplits, numberOfTimeslines, parseManifold)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let manifold = parseManifold text
    print $ numberOfSplits manifold
    print $ numberOfTimeslines manifold
