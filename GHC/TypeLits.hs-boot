{- Declares Nat and Symbol. These must be in GHC.TypeLits because of internal
   machinery. This could perhaps change later. -}
-- TODO: change this?

{-# LANGUAGE EmptyDataDecls #-}

module GHC.TypeLits where

data Nat
data Symbol