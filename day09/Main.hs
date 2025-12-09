module Main where

import           Coordinate  (Coordinate (..))
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Parsers     (parseInt)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let redTiles = map parseRedTile $ T.lines text
    print $ largestRectangle redTiles

parseRedTile :: Text -> Coordinate
parseRedTile text =
  let parts = T.splitOn "," text
   in Coordinate {row = parseInt $ parts !! 1, column = parseInt $ head parts}

rectangleArea :: Coordinate -> Coordinate -> Int
rectangleArea tile1 tile2 = (abs (tile1.row - tile2.row) + 1) * (abs (tile1.column - tile2.column) + 1)

largestRectangle :: [Coordinate] -> Int
largestRectangle tiles = maximum $ map (uncurry rectangleArea) $ pairs tiles
  where
    pairs []     = []
    pairs (x:xs) = map (x, ) xs <> pairs xs
