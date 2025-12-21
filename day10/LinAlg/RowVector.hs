{-# LANGUAGE DerivingVia #-}

module LinAlg.RowVector where

import           GHC.Generics    (Generic)
import           LinAlg.Generics (VectorProduct (..))
import           LinAlg.Vector   (Scalar, Vector)

newtype RowVector = RowVector
  { components :: [Scalar]
  } deriving newtype (Eq)
    deriving stock (Show, Generic)
    deriving (Vector) via VectorProduct RowVector

dimension :: RowVector -> Int
dimension RowVector {..} = length components

zero :: Int -> RowVector
zero dim = RowVector $ replicate dim 0

unit :: Int -> Int -> RowVector
unit dim i =
  RowVector
    [ if i == j
      then 1
      else 0
    | j <- [0 .. dim - 1]
    ]

leading :: RowVector -> Scalar
leading RowVector {..} = head components

project :: RowVector -> RowVector
project RowVector {..} = RowVector $ tail components

embed :: RowVector -> RowVector
embed RowVector {..} = RowVector $ 0 : components

dotProduct :: RowVector -> RowVector -> Scalar
dotProduct (RowVector xs) (RowVector ys) = sum $ zipWith (*) xs ys
