module Main where

import qualified Data.Text        as T
import           Data.Traversable (for)
import           Machine          (configure, parseMachine)
import           Machine2         (configure2, parseMachine2)
import           ProcessFile      (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let machines = map parseMachine $ T.lines text
    print $ sum $ map configure machines
    let machines2 = map parseMachine2 $ T.lines text
    buttonPresses <- for machines2 configure2
    print $ sum buttonPresses
