module Main where

import           Data.List.Split (splitOn)
import qualified Data.Text       as T
import           ProcessFile     (processFile)
import           Puzzle          (easilyFits, impossible, parsePiece,
                                  parsePuzzle)

main :: IO ()
main =
  processFile $ \text -> do
    let parts = splitOn [""] $ T.lines text
        pieces = map parsePiece $ init parts
        puzzles = map (parsePuzzle pieces) $ last parts
        easyPuzzles = filter easilyFits puzzles
        impossiblePuzzles = filter impossible puzzles
    if length easyPuzzles + length impossiblePuzzles < length puzzles
      then putStrLn "Have fun solving Jigsaw puzzles all Christmas..."
      else do
        print $ length easyPuzzles
        putStrLn "The North Pole has been decorated!"
