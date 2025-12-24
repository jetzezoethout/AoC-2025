module Polygon where

import           Coordinate (Coordinate (..), mirror)
import           Direction  (Direction (..), clockWise, moveTowards)
import           Interval   (Interval (..), inside, overlap)
import           Rectangle  (Rectangle, columns, rows)

data Edge
  = HorizontalEdge
      { inRow          :: Int
      , columnInterval :: Interval
      }
  | VerticalEdge
      { inColumn    :: Int
      , rowInterval :: Interval
      }
  deriving (Show)

newtype Polygon = Polygon
  { edges :: [Edge]
  }

draw :: [Coordinate] -> Polygon
draw coords = Polygon $ zipWith makeEdge coords $ tail $ cycle coords
  where
    makeEdge :: Coordinate -> Coordinate -> Edge
    makeEdge vertex1 vertex2
      | vertex1.row == vertex2.row =
        HorizontalEdge {inRow = vertex1.row, columnInterval = Interval vertex1.column vertex2.column}
      | vertex1.column == vertex2.column =
        VerticalEdge {inColumn = vertex1.column, rowInterval = Interval vertex1.row vertex2.row}
      | otherwise = error "cannot draw edge between coordinates not in the same row or column"

hits :: Edge -> Rectangle -> Bool
hits (HorizontalEdge {..}) rect = inRow `inside` rows rect && overlap columnInterval (columns rect)
hits (VerticalEdge {..}) rect = inColumn `inside` columns rect && overlap rowInterval (rows rect)

intersects :: Polygon -> Rectangle -> Bool
intersects Polygon {..} rect = any (`hits` rect) edges

-- | Return the direction towards your left if you're walking along the edge.
leftHandDirection :: Edge -> Direction
leftHandDirection HorizontalEdge {..} =
  if start columnInterval < end columnInterval
    then North
    else South
leftHandDirection VerticalEdge {..} =
  if start rowInterval < end rowInterval
    then East
    else West

connectingVertex :: Edge -> Edge -> Coordinate
connectingVertex (HorizontalEdge {..}) (VerticalEdge {..}) = Coordinate {row = inRow, column = inColumn}
connectingVertex (VerticalEdge {..}) (HorizontalEdge {..}) = Coordinate {row = inRow, column = inColumn}
connectingVertex _ _ = error "Two consecutive edges should make a right angle"

-- | Draw a new polygon just outside the original polygon.
--   Any connected figure containing points both in- and outside the original polygon must intersect this boundary.
boundary :: Polygon -> Polygon
boundary Polygon {..} =
  let (clockWiseTurns, clockWiseVertices, counterClockWiseVertices) = go $ last edges : edges
   in if 2 * clockWiseTurns > length edges
        then draw clockWiseVertices
        else draw counterClockWiseVertices
  where
    go [] = (0, [], [])
    go [_] = (0, [], [])
    go (e1:e2:remaining) =
      let (clockWiseTurns, clockWiseVertices, counterClockWiseVertices) = go $ e2 : remaining
          vertex = connectingVertex e1 e2
          clockWiseDir1 = leftHandDirection e1
          clockWiseDir2 = leftHandDirection e2
          newClockWiseTurns =
            if clockWiseDir2 == clockWise clockWiseDir1
              then clockWiseTurns + 1
              else clockWiseTurns
          clockWiseVertex = vertex `moveTowards` clockWiseDir1 `moveTowards` clockWiseDir2
          counterClockWiseVertex = mirror vertex clockWiseVertex
       in (newClockWiseTurns, clockWiseVertex : clockWiseVertices, counterClockWiseVertex : counterClockWiseVertices)
