{-| This module defines the internals of the representation of type-level
    literals. It should never be imported outside of GHC. -}

{-# LANGUAGE KindSignatures, DataKinds, EmptyDataDecls, TypeFamilies #-}

module GHC.TypeLits.Internals (
  Nat, Symbol, Sing(SNat, SSym)
  ) where

import GHC.Singletons
import {-# SOURCE #-} GHC.TypeLits

--------------------------------------------------------------------------------
newtype instance Sing (n :: Nat)    = SNat Integer
newtype instance Sing (n :: Symbol) = SSym String

--------------------------------------------------------------------------------
