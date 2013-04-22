{-# LANGUAGE PolyKinds, DeriveDataTypeable, NoImplicitPrelude #-}

module Data.Proxy
  (
        Proxy(..)
  ) where

import GHC.Base
import GHC.Show

-- | A concrete, poly-kinded proxy type
data Proxy t = Proxy

-- Eq, Ord, Show, Functor, and Monad instances can be given here without
-- causing import loops. Read, Enum, Bounded, Ix, Applicative, Monoid, Foldable,
-- and Traversable instances are given together with the class declaration.
instance Eq (Proxy s) where
  _ == _ = True

instance Ord (Proxy s) where
  compare _ _ = EQ

instance Show (Proxy s) where
  showsPrec _ _ = showString "Proxy"

instance Functor Proxy where
    fmap _ _ = Proxy
    {-# INLINE fmap #-}

instance Monad Proxy where
    return _ = Proxy
    {-# INLINE return #-}
    _ >>= _ = Proxy
    {-# INLINE (>>=) #-}
