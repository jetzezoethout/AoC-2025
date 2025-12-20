module Main where

import qualified Data.Text   as T
import           Indicator   (configure)
import           Machine     (parseMachine)
import           ProcessFile (processFile)
import           Vector      (configureJoltages)

main :: IO ()
main =
  processFile $ \text -> do
    let machines = map parseMachine $ T.lines text
    print $ sum $ map configure machines
    print $ sum $ map configureJoltages machines
