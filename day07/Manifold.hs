module Manifold where

import           Cached     (Cached, runMemoized, withCache)
import           Coordinate (Coordinate (..))
import           Data.Maybe (fromJust)
import           Data.Set   (Set)
import qualified Data.Set   as S
import           Data.Text  (Text)
import           Direction  (Direction (..), moveTowards)
import           Grid       (Grid (..), atCoordinate, findInGrid, parseGrid)

data ManifoldElement
  = Empty
  | Start
  | Splitter
  deriving (Show, Eq)

parseManifoldElement :: Char -> ManifoldElement
parseManifoldElement '.' = Empty
parseManifoldElement 'S' = Start
parseManifoldElement '^' = Splitter
parseManifoldElement _   = error "Invalid manifold element"

type Manifold = Grid ManifoldElement

parseManifold :: Text -> Manifold
parseManifold = parseGrid parseManifoldElement

start :: Manifold -> Coordinate
start = fromJust . findInGrid (== Start)

numberOfSplits :: Manifold -> Int
numberOfSplits manifold = go 0 $ S.singleton $ start manifold
  where
    go :: Int -> Set Coordinate -> Int
    go splitsSoFar beams
      | below.row == manifold.height = splitsSoFar
      | manifold `atCoordinate` below == Splitter =
        go (splitsSoFar + 1) $ S.insert (below `moveTowards` West) $ S.insert (below `moveTowards` East) remaining
      | otherwise = go splitsSoFar $ S.insert below remaining
      where
        (nextBeam, remaining) = fromJust $ S.minView beams
        below = nextBeam `moveTowards` South

numberOfTimeslines :: Manifold -> Int
numberOfTimeslines manifold = runMemoized $ go $ start manifold
  where
    go :: Coordinate -> Cached Coordinate Int
    go position
      | below.row == manifold.height = return 1
      | manifold `atCoordinate` below == Splitter = do
        leftPaths <- withCache go $ below `moveTowards` West
        rightPaths <- withCache go $ below `moveTowards` East
        return $ leftPaths + rightPaths
      | otherwise = withCache go below
      where
        below = position `moveTowards` South
