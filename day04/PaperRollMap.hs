module PaperRollMap where

import           Coordinate  (Coordinate (..), addCoordinate)
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Text   (Text)
import           LocatedChar (LocatedChar (..), locateText)

type PaperRollMap = Set Coordinate

parsePaperRollMap :: Text -> Set Coordinate
parsePaperRollMap text = S.fromList [location locatedChar | locatedChar <- locateText text, char locatedChar == '@']

neighbours :: Coordinate -> [Coordinate]
neighbours coord = map (coord `addCoordinate`) deltas
  where
    deltas = [Coordinate row col | row <- [-1, 0, 1], col <- [-1, 0, 1], not (row == 0 && col == 0)]

isFree :: PaperRollMap -> Coordinate -> Bool
isFree paperRollMap coord = length (filter (`S.member` paperRollMap) $ neighbours coord) < 4

freeRolls :: PaperRollMap -> Int
freeRolls paperRollMap = length $ filter (isFree paperRollMap) $ S.toList paperRollMap

cleanUpOnce :: PaperRollMap -> PaperRollMap
cleanUpOnce paperRollMap = S.foldr process S.empty paperRollMap
  where
    process :: Coordinate -> PaperRollMap -> PaperRollMap
    process roll acc =
      if isFree paperRollMap roll
        then acc
        else S.insert roll acc

cleanUp :: PaperRollMap -> PaperRollMap
cleanUp = go
  where
    go paperRollMap =
      let cleanedMap = cleanUpOnce paperRollMap
       in if S.size paperRollMap == S.size cleanedMap
            then paperRollMap
            else go cleanedMap
