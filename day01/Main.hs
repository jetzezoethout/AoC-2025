module Main where

import qualified Data.Text   as T
import           Dial        (doRotations, parseDialRotation)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let rotations = map parseDialRotation $ T.lines text
        dialPositions = doRotations rotations
        zeroes = length $ filter (== 0) dialPositions
    print zeroes
