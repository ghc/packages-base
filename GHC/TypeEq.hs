{- | This module includes definitions related to type-level equality -}

{-# LANGUAGE EmptyCase, PolyKinds, TypeOperators, EmptyDataDecls,
             GADTs #-}

module GHC.TypeEq (
  Void, absurd, Refuted, Decision,
  (:~:)(..)
  ) where

-- Note: This is similar to the "void" package and the "type-eq" package

-- | an empty type
data Void

-- | eliminate an empty type to produce anything
absurd :: Void -> a
absurd x = case x of {} 

-- | The type of a witness that another type is uninhabited
type Refuted a = a -> Void

-- | The type of a witness that a type is either inhabited or not
type Decision a = Either (Refuted a) a

--------------------------------------------------------------------------------
-- | A type that provides evidence for equality between two types.
data (:~:) :: k -> k -> * where
  Refl :: a :~: a

instance Show (a :~: b) where
  show Refl = "Refl"


