--    ┌─────┐  ┌─────┐  ┌─────┐  ┌─────┐
-- ──▶│  1  ├─▶│  2  ├─▶│  …  ├─▶│  n  ├─▶
--    └─────┘  └─────┘  └─────┘  └─────┘

{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.HashCores.Composition.Pipelined (
  Pipelined(..),
  )
  where

import           Clash.Prelude

import           Data.Proxy
import           Data.Singletons.Prelude           (type (@@), Apply, TyFun)

import           Clash.HashCores.Class.Composition
import           Clash.HashCores.Class.Iterable

data Pipelined = Pipelined deriving Show

data PipelineStep :: Domain -> Nat -> Nat -> * -> TyFun Nat * -> *
type instance Apply (PipelineStep domain t0 delay s) i
  = DSignal domain t0 s
  -> DSignal domain (t0+(delay*(1+i))) s

instance Default Pipelined where
  def = Pipelined

instance (Iterable iterable _i s _o r d) => Composition Pipelined iterable 'Always _i s _o r d where
  indexedCompose :: forall domain gated synchronous reference .
      ( HiddenClockReset domain gated synchronous)
      => Pipelined
      -> iterable
      -- | In signal
      -> DSignal domain reference s
      -- | Out signal
      -> ( DSignal domain  reference        ()
         , DSignal domain (reference + r*d) s  )

  indexedCompose Pipelined iterable input = (pure (),) $
        dfold (Proxy @(PipelineStep domain reference _ s))
          pipe
          (oneStep iterable 0)
          (reverse . drop d1 . indices $ SNat @(1+(r-1)))
      $ input
    where
      pipe :: SNat l -> Index r
        -> PipelineStep domain reference d s @@ l
        -> PipelineStep domain reference d s @@ (l+1)
      pipe _ i f' = oneStep iterable (pure i) . f'
