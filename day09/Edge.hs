module Edge where

import           Coordinate (Coordinate (..))
import           Interval   (Interval, makeInterval, overlapStrictly,
                             strictlyInside)
import           Rectangle  (Rectangle (..), columns, rows)

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
eliminatesHorizontal rect HorizontalEdge {..} =
  inRow `strictlyInside` rows rect && overlapStrictly columnInterval (columns rect)

eliminatesVertical :: Rectangle -> VerticalEdge -> Bool
eliminatesVertical rect VerticalEdge {..} =
  inColumn `strictlyInside` columns rect && overlapStrictly rowInterval (rows rect)

eliminates :: Rectangle -> Edge -> Bool
eliminates rect (Left horEdge)  = eliminatesHorizontal rect horEdge
eliminates rect (Right verEdge) = eliminatesVertical rect verEdge

isInside :: [Edge] -> Rectangle -> Bool
isInside edges rect = not $ any (eliminates rect) edges
