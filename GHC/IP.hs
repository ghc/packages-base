{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
module GHC.IP where

import GHC.TypeLits

-- | This class is used to implement implicit parameters.
class IP (name :: Symbol) t | name -> t where
  ipValue :: t


