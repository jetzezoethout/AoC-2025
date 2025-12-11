module Main where

import           Graph       (numberOfPaths, numberOfProblematicPaths,
                              parseGraph)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let graph = parseGraph text
    print $ numberOfPaths graph "you" "out"
    print $ numberOfProblematicPaths graph
