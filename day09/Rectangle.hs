module Rectangle where

import           Coordinate (Coordinate (..))
import           Interval   (Interval (..))

data Rectangle = Rectangle
  { topLeft     :: Coordinate
  , bottomRight :: Coordinate
  } deriving (Show)

rows :: Rectangle -> Interval
rows Rectangle {..} = Interval topLeft.row bottomRight.row

columns :: Rectangle -> Interval
columns Rectangle {..} = Interval topLeft.column bottomRight.column

makeRectangle :: Coordinate -> Coordinate -> Rectangle
makeRectangle vertex1 vertex2 =
  Rectangle
    { topLeft = Coordinate {row = min vertex1.row vertex2.row, column = min vertex1.column vertex2.column}
    , bottomRight = Coordinate {row = max vertex1.row vertex2.row, column = max vertex1.column vertex2.column}
    }

makeRectangles :: [Coordinate] -> [Rectangle]
makeRectangles = go
  where
    go []           = []
    go (tile:tiles) = map (makeRectangle tile) tiles <> go tiles

area :: Rectangle -> Int
area Rectangle {..} = (bottomRight.row - topLeft.row + 1) * (bottomRight.column - topLeft.column + 1)
