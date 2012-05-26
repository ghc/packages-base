{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
module GHC.IP (IP, IPName(..), IPValue, ipUse, ipDef) where

import GHC.TypeLits

-- | A singleton type used to name implicit parameters.
data IPName (name :: Symbol) = IPName

-- | A type used to give values to implicit parameters.
-- The name is a phantom parameter because it needs no run-time representation.
newtype IPValue (name :: Symbol) a = IPValue a


-- | The syntax @?x@ is desuagred into @ipUse (IPName :: "x")@
ipUse :: IP x a => IPName x -> a
ipUse x = case val x of
            IPValue a -> a
  where val :: IP x a => IPName x -> IPValue x a
        val _ = ip

-- | This is used when we construct evidence for the 'IP' class.
-- @let ?x = e in ...@ uses @ipDef (IPName :: "x") e@ to cinstruct
-- a new implict parameter.
ipDef :: IPName x -> a -> IPValue x a
ipDef _ a = IPValue a

-- | The syntax @?x :: a@ is desugared into @IP "x" a@
class IP x a | x -> a where
  ip :: IPValue x a


