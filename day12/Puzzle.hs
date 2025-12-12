module Puzzle where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

newtype Piece = Piece
  { numberOfTiles :: Int
  } deriving (Show)

parsePiece :: [Text] -> Piece
parsePiece textLines = Piece $ sum $ map tilesOnRow $ drop 1 textLines
  where
    tilesOnRow textLine = length $ filter (== '#') $ T.unpack textLine

data Puzzle = Puzzle
  { width           :: Int
  , height          :: Int
  , availablePieces :: [Piece]
  , pieceAmounts    :: [Int]
  } deriving (Show)

parsePuzzle :: [Piece] -> Text -> Puzzle
parsePuzzle pieces text =
  let parts = T.splitOn ": " text
      dimensionParts = T.splitOn "x" $ head parts
   in Puzzle
        { width = parseInt $ head dimensionParts
        , height = parseInt $ dimensionParts !! 1
        , availablePieces = pieces
        , pieceAmounts = map parseInt $ T.words $ parts !! 1
        }

availableTiles :: Puzzle -> Int
availableTiles Puzzle {..} = width * height

availablePieceGrids :: Puzzle -> Int
availablePieceGrids Puzzle {..} = (width `div` 3) * (height `div` 3)

requiredPieces :: Puzzle -> Int
requiredPieces Puzzle {..} = sum pieceAmounts

requiredTiles :: Puzzle -> Int
requiredTiles Puzzle {..} = sum $ zipWith (*) pieceAmounts $ map numberOfTiles availablePieces

easilyFits :: Puzzle -> Bool
easilyFits puzzle = availablePieceGrids puzzle >= requiredPieces puzzle

impossible :: Puzzle -> Bool
impossible puzzle = availableTiles puzzle < requiredTiles puzzle
