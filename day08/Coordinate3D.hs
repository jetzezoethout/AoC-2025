module Coordinate3D where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

data Coordinate3D = Coordinate3D
  { x :: Int
  , y :: Int
  , z :: Int
  } deriving (Eq, Ord, Show)

parseCoordinate3D :: Text -> Coordinate3D
parseCoordinate3D text =
  let parts = T.splitOn "," text
   in Coordinate3D {x = parseInt $ head parts, y = parseInt $ parts !! 1, z = parseInt $ parts !! 2}

squaredDistance :: Coordinate3D -> Coordinate3D -> Int
squaredDistance coord1 coord2 =
  square (abs $ coord1.x - coord2.x) + square (abs $ coord1.y - coord2.y) + square (abs $ coord1.z - coord2.z)
  where
    square x = x * x
