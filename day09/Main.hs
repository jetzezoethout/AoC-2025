module Main where

import           Coordinate    (Coordinate (..))
import           Data.Function (on)
import           Data.List     (sortBy)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Edge          (isInside, makeEdges)
import           Parsers       (parseInt)
import           ProcessFile   (processFile)
import           Rectangle     (area, makeRectangles)

main :: IO ()
main =
  processFile $ \text -> do
    let redTiles = map parseRedTile $ T.lines text
        sortedRectangles = sortBy (flip compare `on` area) $ makeRectangles redTiles
        edges = makeEdges redTiles
    print $ area $ head sortedRectangles
    print $ area $ head $ filter (isInside edges) sortedRectangles

parseRedTile :: Text -> Coordinate
parseRedTile text =
  let parts = T.splitOn "," text
   in Coordinate {row = parseInt $ parts !! 1, column = parseInt $ head parts}
