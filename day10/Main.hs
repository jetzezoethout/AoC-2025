module Main where

import qualified Data.Text   as T
import           Indicator   (configureIndicators)
import           Joltages    (configureJoltages)
import           Machine     (parseMachine)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let machines = map parseMachine $ T.lines text
    print $ sum $ map configureIndicators machines
    print $ sum $ map configureJoltages machines
