-- A pipeline construction in which each stage is a separate component delayed
-- according to the MerkleDamgard compression function definition used at
-- instantiation.
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

-- | Pipeline that has @Rounds hash@ number of instantiated units, giving a
-- throughput of 1 hash per cycle.
data Pipelined = Pipelined deriving Show

data PipelineStep :: Domain -> Nat -> Nat -> * -> TyFun Nat * -> *
type instance Apply (PipelineStep domain t0 delay s) i
  = DSignal domain t0 s
  -> DSignal domain (t0+(delay*(1+i))) s

instance Default Pipelined where
  def = Pipelined

-- | Pipeline that has @Rounds hash@ number of instantiated units, giving a
-- throughput of 1 hash per cycle.
instance Composition Pipelined 'Typed where
  indexedCompose ::
    forall domain gated synchronous t0 t1 x xn1 a .
    ( HiddenClockReset domain gated synchronous
    , KnownNat xn1
    , 1 <= x
    , xn1 ~ (x - 1))
    => Pipelined
    -> ( forall t0'. DSignal domain t0' (Index x)
        -> DSignal domain t0' a
        -> DSignal domain (t0'+t1) a
       )
    -> DSignal domain t0 a            -- In
    -> DSignal domain (t0+(x*t1)) a   -- Out
  indexedCompose Pipelined f input =
        dfold (Proxy @(PipelineStep domain t0 _ a))
          pipe
          (f 0)
          (reverse . drop d1 . indices $ SNat @(1+xn1))
      $ input
    where
      pipe :: SNat l -> Index x
        -> PipelineStep domain t0 t1 a @@ l
        -> PipelineStep domain t0 t1 a @@ (l+1)
      pipe _ i f' = f (pure i) . f'
