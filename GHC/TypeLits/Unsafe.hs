{-| This module defines functions for use with GHC.TypeLits that are
    inherently unsafe. Because these unsafe operations depend critically
    on constructors that are not exported from the safe module -}

{-# LANGUAGE KindSignatures, DataKinds #-}

module GHC.TypeLits.Unsafe (
  unsafeSingNat, unsafeSingSymbol
  ) where

import GHC.TypeLits.Internals

unsafeSingNat :: Integer -> Sing (n :: Nat)
unsafeSingNat = SNat

unsafeSingSymbol :: String -> Sing (n :: Symbol)
unsafeSingSymbol = SSym
