module Main where

import qualified Data.Text        as T
import           Data.Traversable (for)
import           Indicator        (configure)
import           Joltages         (configure2)
import           Machine          (parseMachine)
import           ProcessFile      (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let machines = map parseMachine $ T.lines text
    print $ sum $ map configure machines
    buttonPresses <- for machines configure2
    print $ sum buttonPresses
