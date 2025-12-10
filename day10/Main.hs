module Main where

import qualified Data.Text   as T
import           Machine     (configure, parseMachine)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let machines = map parseMachine $ T.lines text
    print $ sum $ map configure machines
