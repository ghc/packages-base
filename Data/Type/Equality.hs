{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Type.Equality where

import Data.Data
import Data.Ix
import GHC.Generics
import Control.Exception
import Control.Category


data a :=: b where
  Refl :: a :=: a

-- TODO: Add fixity declaration for (:=:)

-- with credit to Conal Elliott for 'ty', Erik Hesselink & Martijn van
-- Steenbergen for 'type-equality', Edward Kmett for 'eq', and Gabor Greif
-- for 'type-eq'
sym :: (a :=: b) -> (b :=: a)
sym Refl = Refl

trans :: (a :=: b) -> (b :=: c) -> (a :=: c)
trans Refl Refl = Refl

coerce :: (a :=: b) -> a -> b
coerce Refl = Prelude.id

liftEq :: (a :=: b) -> (f a :=: f b)
liftEq Refl = Refl

liftEq2 :: (a :=: a') -> (b :=: b') -> (f a b :=: f a' b')
liftEq2 Refl Refl = Refl

liftEq3 :: (a :=: a') -> (b :=: b') -> (c :=: c') -> (f a b c :=: f a' b' c')
liftEq3 Refl Refl Refl = Refl

liftEq4 :: (a :=: a') -> (b :=: b') -> (c :=: c') -> (d :=: d')
        -> (f a b c d :=: f a' b' c' d')
liftEq4 Refl Refl Refl Refl = Refl

lower :: (f a :=: f b) -> a :=: b
lower Refl = Refl

deriving instance Eq   (a :=: b)
deriving instance Show (a :=: b)
deriving instance Ord  (a :=: b)

instance Read (a :=: a) where -- TODO: is this correct?
  readsPrec d = readParen (d > 10) (\r -> [(Refl, s) | ("Refl",s) <- lex r ])

instance Category (:=:) where
  id          = Refl
  Refl . Refl = Refl

-- TODO: more instances?

-- | A logically uninhabited data type.
data Void
  deriving Typeable
-- instances as in Edward Kmett's 'void' package

deriving instance Data    Void
deriving instance Generic Void

instance Eq Void where
  _ == _ = True

instance Ord Void where
  compare _ _ = EQ

instance Show Void where
  showsPrec _ = absurd

-- | Reading a 'Void' value is always a parse error, considering 'Void' as
-- a data type with no constructors.
instance Read Void where
  readsPrec _ _ = []

instance Ix Void where
  range     _ = []
  index     _ = absurd
  inRange   _ = absurd
  rangeSize _ = 0

instance Exception Void

-- | Since 'Void' values logically don't exist, this witnesses the logical
-- reasoning tool of \"ex falso quodlibet\".
absurd :: Void -> a
absurd a = a `seq` undefined

type Refuted a = a -> Void
data Decision a = Proved a
                | Disproved (Refuted a)

class EqT f where
 eqT :: f a -> f b -> Maybe (a :=: b)

class EqT f => DecideEqT f where
 decideEqT :: f a -> f b -> Decision (a :=: b)

-- for easy writing of EqT instances
defaultEqT :: DecideEqT f => f a -> f b -> Maybe (a :=: b)
defaultEqT = undefined

instance EqT ((:=:) a) where
  eqT Refl Refl = Just Refl

instance DecideEqT ((:=:) a) where
  decideEqT Refl Refl = Proved Refl
