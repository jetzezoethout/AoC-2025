{-# LANGUAGE DerivingVia #-}

module LinAlg.Expression where

import           GHC.Generics     (Generic)
import           LinAlg.Generics  (VectorProduct (..))
import           LinAlg.RowVector (RowVector, dimension, dotProduct, embed,
                                   unit, zero)
import           LinAlg.Vector    (Scalar, Vector)

-- | An affine linear expression depending on a number of parameters.
--  `Expression`s can be added and multiplied by a `Scalar`, so they are `Vector`s.
data Expression = Expression
  { paramCoefficients :: RowVector
    -- ^ The coefficients of the parameters in the expression.
  , constant          :: Scalar
    -- ^ The constant part of the expression.
  } deriving stock (Eq, Generic)
    deriving (Vector) via (VectorProduct Expression)

-- | Introduce a new (unused) parameter x0, shifting all existing parameters one place to the right.
shiftExpression :: Expression -> Expression
shiftExpression Expression {..} = Expression {paramCoefficients = embed paramCoefficients, constant = constant}

constExpression :: Int -> Scalar -> Expression
constExpression numParams = Expression (zero numParams)

paramExpression :: Int -> Int -> Expression
paramExpression numParams param = Expression {paramCoefficients = unit numParams param, constant = 0}

evaluate :: Expression -> RowVector -> Scalar
evaluate Expression {..} values =
  if dimension values /= dimension paramCoefficients
    then error "Number of values does not match the number of parameters"
    else dotProduct values paramCoefficients + constant
