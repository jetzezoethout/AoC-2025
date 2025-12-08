module Edge where

import           Coordinate3D (Coordinate3D, squaredDistance)
import           Data.Map     (Map, (!))
import qualified Data.Map     as M

data Edge = Edge
  { squaredLength :: Int
  , from          :: Coordinate3D
  , to            :: Coordinate3D
  } deriving (Show)

makeEdge :: Coordinate3D -> Coordinate3D -> Edge
makeEdge coord1 coord2 = Edge {squaredLength = squaredDistance coord1 coord2, from = coord1, to = coord2}

generateEdges :: [Coordinate3D] -> [Edge]
generateEdges coords = map (uncurry makeEdge) $ pairs coords
  where
    pairs []     = []
    pairs (x:xs) = map (x, ) xs <> pairs xs

componentSizes :: [Coordinate3D] -> [Edge] -> Int -> [Int]
componentSizes coordinates edges steps =
  go (M.fromList $ zip coordinates [0 ..]) (M.fromList $ zip [0 ..] [1 | _ <- coordinates]) $ take steps edges
  where
    go :: Map Coordinate3D Int -> Map Int Int -> [Edge] -> [Int]
    go _ componentSizesMap [] = M.elems componentSizesMap
    go componentMap componentSizesMap (Edge {..}:remaining) =
      let sourceComponent = componentMap ! from
          targetComponent = componentMap ! to
          sourceSize = componentSizesMap ! sourceComponent
          targetSize = componentSizesMap ! targetComponent
          updateComponent comp =
            if comp == targetComponent
              then sourceComponent
              else comp
       in if sourceComponent == targetComponent
            then go componentMap componentSizesMap remaining
            else go
                   (M.map updateComponent componentMap)
                   (M.delete targetComponent $ M.insert sourceComponent (sourceSize + targetSize) componentSizesMap)
                   remaining

findFinalEdge :: [Coordinate3D] -> [Edge] -> Edge
findFinalEdge coordinates = go (M.fromList $ zip coordinates [0 ..]) (M.fromList $ zip [0 ..] [1 | _ <- coordinates])
  where
    go :: Map Coordinate3D Int -> Map Int Int -> [Edge] -> Edge
    go _ _ [] = error "out of edges"
    go componentMap componentSizesMap (edge@Edge {..}:remaining) =
      let sourceComponent = componentMap ! from
          targetComponent = componentMap ! to
          sourceSize = componentSizesMap ! sourceComponent
          targetSize = componentSizesMap ! targetComponent
          updateComponent comp =
            if comp == targetComponent
              then sourceComponent
              else comp
       in if sourceComponent == targetComponent
            then go componentMap componentSizesMap remaining
            else let updatedComponentSizes =
                       M.delete targetComponent $ M.insert sourceComponent (sourceSize + targetSize) componentSizesMap
                  in if M.size updatedComponentSizes == 1
                       then edge
                       else go (M.map updateComponent componentMap) updatedComponentSizes remaining
