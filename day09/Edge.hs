module Edge where

import           Coordinate (Coordinate (..))
import           Interval   (Interval, collide, makeInterval, overlap)
import           Rectangle  (Rectangle (..), bottom, left, right, top)

data HorizontalEdge = HorizontalEdge
  { inRow          :: Int
  , columnInterval :: Interval
  } deriving (Show)

data VerticalEdge = VerticalEdge
  { inColumn    :: Int
  , rowInterval :: Interval
  } deriving (Show)

type Edge = Either HorizontalEdge VerticalEdge

makeEdge :: Coordinate -> Coordinate -> Edge
makeEdge vertex1 vertex2 =
  if vertex1.row == vertex2.row
    then Left $ HorizontalEdge {inRow = vertex1.row, columnInterval = makeInterval vertex1.column vertex2.column}
    else Right $ VerticalEdge {inColumn = vertex1.column, rowInterval = makeInterval vertex1.row vertex2.row}

makeEdges :: [Coordinate] -> [Edge]
makeEdges coords = zipWith makeEdge coords $ tail $ cycle coords

eliminatesHorizontal :: Rectangle -> HorizontalEdge -> Bool
eliminatesHorizontal rect HorizontalEdge {..}
  | inRow == top rect = collide columnInterval $ makeInterval (left rect) (right rect)
  | top rect < inRow && inRow < bottom rect = overlap columnInterval $ makeInterval (left rect) (right rect)
  | inRow == bottom rect = collide columnInterval $ makeInterval (right rect) (left rect)
  | otherwise = False

eliminatesVertical :: Rectangle -> VerticalEdge -> Bool
eliminatesVertical rect VerticalEdge {..}
  | inColumn == left rect = collide rowInterval $ makeInterval (bottom rect) (top rect)
  | left rect < inColumn && inColumn < right rect = overlap rowInterval $ makeInterval (top rect) (bottom rect)
  | inColumn == right rect = collide rowInterval $ makeInterval (top rect) (bottom rect)
  | otherwise = False

eliminates :: Rectangle -> Edge -> Bool
eliminates rect (Left horEdge)  = eliminatesHorizontal rect horEdge
eliminates rect (Right verEdge) = eliminatesVertical rect verEdge

isInside :: [Edge] -> Rectangle -> Bool
isInside edges rect = not $ any (eliminates rect) edges
