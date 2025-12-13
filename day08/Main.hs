module Main where

import           Components   (componentSizes, connectEdges, discrete,
                               findFinalEdge)
import           Coordinate3D (parseCoordinate3D)
import           Data.List    (sort, sortBy)
import qualified Data.Text    as T
import           Edge         (generateEdges, wallScore)
import           ProcessFile  (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let coordinates = map parseCoordinate3D $ T.lines text
        start = discrete coordinates
        sortedEdges = sort $ generateEdges coordinates
    print $ product $ take 3 $ sortDesc $ componentSizes $ connectEdges start $ take 1000 sortedEdges
    print $ wallScore $ findFinalEdge start sortedEdges

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy $ flip compare
