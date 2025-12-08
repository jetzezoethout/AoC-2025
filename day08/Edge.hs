module Edge where

import           Coordinate3D  (Coordinate3D (..), squaredDistance)
import           Data.Function (on)

data Edge = Edge
  { source :: Coordinate3D
  , target :: Coordinate3D
  } deriving (Show, Eq)

instance Ord Edge where
  compare :: Edge -> Edge -> Ordering
  compare = compare `on` squaredLength

squaredLength :: Edge -> Int
squaredLength Edge {..} = squaredDistance source target

generateEdges :: [Coordinate3D] -> [Edge]
generateEdges coords = map (uncurry Edge) $ pairs coords
  where
    pairs []     = []
    pairs (x:xs) = map (x, ) xs <> pairs xs

wallScore :: Edge -> Int
wallScore Edge {..} = source.x * target.x
