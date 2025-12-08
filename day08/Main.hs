module Main where

import           Coordinate3D  (parseCoordinate3D)
import           Data.Function (on)
import           Data.List     (sort, sortBy)
import qualified Data.Text     as T
import           Edge          (Edge (squaredLength), componentSizes,
                                generateEdges)
import           ProcessFile   (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let coordinates = map parseCoordinate3D $ T.lines text
        sortedEdges = sortBy (compare `on` squaredLength) $ generateEdges coordinates
    print $ product $ take 3 $ sortBy (flip compare) $ componentSizes coordinates sortedEdges 1000
