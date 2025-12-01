module Main where

import qualified Data.Text    as T
import           Dial         (Dial (..), doRotations, isAtZero)
import           DialRotation (parseDialRotation)
import           ProcessFile  (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let rotations = map parseDialRotation $ T.lines text
        dialPositions = doRotations rotations
    print $ length $ filter isAtZero dialPositions
    print $ zeroesPassed $ last dialPositions
