{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.TypeNats
import Data.Type.Equality
import Data.Proxy
import Data.Vector as V
import qualified Data.Kind as K
import Data.Reflection as Refl

data Vec (n :: Nat) a = Vec { getVector :: V.Vector a } deriving (Eq, Show, Ord)

data Po2Vec :: K.* -> K.* where
  MkPo2Vec :: IsPo2 n ~ True => Vec n a -> Po2Vec (Vec n a)

mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec v | V.length v == l = Just (Vec v)
        | otherwise       = Nothing
  where
    l = fromIntegral (natVal (Proxy @n))

deriving instance Show a => Show (Po2Vec (Vec n a))
deriving instance Eq a => Eq (Po2Vec (Vec n a))

type family IsPo2 (t :: Nat) :: Bool where
  IsPo2 n = 2 ^ (Log2 n) == n


v = mkVec @3 (V.fromList [1,2])

