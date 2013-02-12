{-| This module defines support for singletons in GHC. -}

{-# LANGUAGE PolyKinds, TypeFamilies, RankNTypes, DataKinds,
             TypeOperators, FlexibleContexts, GADTs,
             FlexibleInstances, UndecidableInstances #-}

module GHC.Singletons (
  OfKind(..), KindOf,
  Sing, SingI(..), SingE(..),
  SingRep, Demote,
  withSing, singThat,
  SingInstance(..), SingKind(..), SingEquality(..),
  showsPrecSing, readsPrecSing
  ) where

import GHC.TypeEq

-- | (Kind) A kind useful for passing kinds as parameters.
data OfKind (a :: *) = KindParam

{- | A shortcut for naming the kind parameter corresponding to the
kind of a some type.  For example, @KindOf Int ~ (KindParam :: OfKind *)@,
but @KindOf 2 ~ (KindParam :: OfKind Nat)@. -}
type KindOf (a :: k) = (KindParam :: OfKind k)

-- | The data family defining singletons
data family Sing (n :: k)

-- | The class 'SingI' provides a \"smart\" constructor for singleton types.
-- There are built-in instances for the singleton types corresponding
-- to type literals.
class SingI a where
  -- | The only value of type @Sing a@
  sing :: Sing a

{- | A class that converts singletons of a given kind into values of some
representation type (i.e., we "forget" the extra information carried
by the singletons, and convert them to ordinary values).

Note that 'fromSing' is overloaded based on the /kind/ of the values
and not their type---all types of a given kind are processed by the
same instances.
-}
class (kparam ~ KindParam) => SingE (kparam :: OfKind k) where
  type DemoteRep kparam :: *
  fromSing :: Sing (a :: k) -> DemoteRep kparam

{- | A convenience class, useful when we need to both introduce and eliminate
a given singleton value. Users should never need to define instances of
this classes. -}
class    (SingI a, SingE (KindOf a)) => SingRep (a :: k)
instance (SingI a, SingE (KindOf a)) => SingRep (a :: k)

{- | A convenient name for the type used to representing the values
for a particular singleton family.  For example, @Demote 2 ~ Integer@,
and also @Demote 3 ~ Integer@, but @Demote "Hello" ~ String@. -}
type Demote a = DemoteRep (KindOf a)

{- | A convenience function useful when we need to name a singleton value
multiple times.  Without this function, each use of 'sing' could potentially
refer to a different singleton, and one has to use type signatures to
ensure that they are the same. -}
withSing :: SingI a => (Sing a -> b) -> b
withSing f = f sing

{- | A convenience function that names a singleton satisfying a certain
property.  If the singleton does not satisfy the property, then the function
returns 'Nothing'. The property is expressed in terms of the underlying
representation of the singleton. -}
singThat :: SingRep a => (Demote a -> Bool) -> Maybe (Sing a)
singThat p = withSing $ \x -> if p (fromSing x) then Just x else Nothing


{- | An object of type @SingInstance a@ contains an implicit singleton
     of type @a@ -}
data SingInstance (a :: k) where
  SingInstance :: SingRep a => SingInstance a

{- | The SingKind class includes all kinds that have an associated singleton -}
class (kparam ~ KindParam) => SingKind (kparam :: OfKind k) where
  singInstance :: forall (a :: k). Sing a -> SingInstance a

-- | The SingEquality class includes all kinds that have a decidable equality.
-- The minimal complete definition is just @decideSing@, but implementors may
-- choose just to write @sameSing@ instead, though this gives weaker guarantees.
class SingKind (kparam :: OfKind k) => SingEquality (kparam :: OfKind k) where
  sameSing :: Sing (a :: k) -> Sing (b :: k) -> Maybe (a :~: b)
  sameSing a b = case decideSing a b of
                   Right witness -> Just witness
                   otherwise     -> Nothing

  decideSing :: Sing (a :: k) -> Sing (b :: k) -> Decision (a :~: b)

{- | showsPrecSing is a good default implementation for Show instances of
     singletons. It produces the same string as would the original datatype
     upon which a singleton is built. -}
showsPrecSing :: (SingE (KindOf a), Show (Demote a)) => Int -> Sing a -> ShowS
showsPrecSing p = showsPrec p . fromSing

{- | readsPrecSing is a good default implementation for Read instances of
     singletons. It parses the same string as would the original datatype
     upon which a singleton is built. -}
readsPrecSing :: (SingRep a, Read (Demote a), Eq (Demote a))
              => Int -> ReadS (Sing a)
readsPrecSing p cs = do (x,ys) <- readsPrec p cs
                        case singThat (== x) of
                          Just y  -> [(y,ys)]
                          Nothing -> []

