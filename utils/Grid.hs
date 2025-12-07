module Grid where

import           Coordinate  (Coordinate (..))
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Data.Vector (Vector, fromList, (!))
import qualified Data.Vector as V

type DefaultArray = Vector Int

data Grid a = Grid
  { height :: Int
  , width  :: Int
  , grid   :: Vector (Vector a)
  } deriving (Show)

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f Grid {..} =
    Grid {height = height, width = width, grid = fmap (fmap f) grid}

fromNestedList :: [[a]] -> Grid a
fromNestedList xxs =
  Grid
    { height = length xxs
    , width = length $ head xxs
    , grid = fromList $ map fromList xxs
    }

parseGrid :: (Char -> a) -> Text -> Grid a
parseGrid parser text =
  fromNestedList $ map (map parser . T.unpack) $ T.lines text

atCoordinate :: Grid a -> Coordinate -> a
Grid {..} `atCoordinate` Coordinate {..} = (grid ! row) ! column

safeAtCoordinate :: Grid a -> Coordinate -> Maybe a
grid `safeAtCoordinate` coordinate =
  [grid `atCoordinate` coordinate | coordinate `isInside` grid]

isInside :: Coordinate -> Grid a -> Bool
Coordinate {..} `isInside` Grid {..} =
  0 <= row && row < height && 0 <= column && column < width

findInGrid :: (a -> Bool) -> Grid a -> Maybe Coordinate
findInGrid p Grid {..} = go 0 grid
  where
    go rowsChecked rows =
      if V.null rows
        then Nothing
        else case V.findIndex p (rows ! 0) of
               Nothing     -> go (rowsChecked + 1) $ V.drop 1 rows
               Just column -> Just $ Coordinate rowsChecked column
