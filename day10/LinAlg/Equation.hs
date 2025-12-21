{-# LANGUAGE DerivingVia #-}

module LinAlg.Equation where

import           GHC.Generics     (Generic)
import           LinAlg.Generics  (VectorProduct (..))
import           LinAlg.RowVector (RowVector, dimension, leading, project)
import           LinAlg.Vector    (Scalar, Vector)

-- | An affine linear equation in several unknowns. `Equation`s can be added and multiplied by a `Scalar`, so they are `Vector`s.
data Equation = Equation
  { eqCoefficients :: RowVector
    -- ^ The coefficients of the unknowns on the left-hand side of the equation.
  , rhs            :: Scalar
    -- ^ The scalar on the right-hand side of the equation.
  } deriving stock (Eq, Generic)
    deriving (Vector) via VectorProduct Equation

dropFirst :: Equation -> Equation
dropFirst Equation {..} =
  if leading eqCoefficients /= 0
    then error "Drop leading variable with nonzero coefficient"
    else Equation {eqCoefficients = project eqCoefficients, rhs = rhs}

data System = System
  { numVars :: Int
  , eqs     :: [Equation]
  }

system :: Int -> [Equation] -> System
system numVars eqs =
  if any ((/= numVars) . dimension . eqCoefficients) eqs
    then error "Inconsistent dimensions"
    else System numVars eqs
