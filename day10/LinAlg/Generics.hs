{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}

module LinAlg.Generics where

import           GHC.Generics  (Generic (..), K1 (..), M1 (..), (:*:) (..))
import           LinAlg.Vector (Scalar, Vector (..))

class GVector a where
  gAdd :: a p -> a p -> a p
  gMul :: Scalar -> a p -> a p

instance GVector a => GVector (M1 tag meta a) where
  gAdd :: GVector a => M1 tag meta a p -> M1 tag meta a p -> M1 tag meta a p
  gAdd (M1 x) (M1 y) = M1 $ gAdd x y
  gMul :: GVector a => Scalar -> M1 tag meta a p -> M1 tag meta a p
  gMul l (M1 x) = M1 $ gMul l x

instance (GVector a, GVector b) => GVector (a :*: b) where
  gAdd :: (GVector a, GVector b) => (a :*: b) p -> (a :*: b) p -> (a :*: b) p
  gAdd (xa :*: xb) (ya :*: yb) = gAdd xa ya :*: gAdd xb yb
  gMul :: (GVector a, GVector b) => Scalar -> (a :*: b) p -> (a :*: b) p
  gMul l (xa :*: xb) = gMul l xa :*: gMul l xb

instance Vector v => GVector (K1 tag v) where
  gAdd :: Vector v => K1 tag v p -> K1 tag v p -> K1 tag v p
  gAdd (K1 x) (K1 y) = K1 $ x ^+ y
  gMul :: Vector v => Scalar -> K1 tag v p -> K1 tag v p
  gMul l (K1 x) = K1 $ l ^* x

-- | Wrapper newtype that allows the client to derive `Vector` on any (non-empty) `Generic` product of `Vector`s.
newtype VectorProduct v =
  VectorProduct v
  deriving newtype (Eq)

instance (Eq v, Generic v, GVector (Rep v)) => Vector (VectorProduct v) where
  (^+) :: (Eq v, Generic v) => VectorProduct v -> VectorProduct v -> VectorProduct v
  VectorProduct x ^+ VectorProduct y = VectorProduct $ to $ gAdd (from x) (from y)
  (^*) :: (Eq v, Generic v) => Scalar -> VectorProduct v -> VectorProduct v
  l ^* VectorProduct x = VectorProduct $ to $ gMul l (from x)
