--    ┌─────┐  ┌─────┐  ┌─────┐  ┌─────┐
-- ──▶│  1  ├─▶│  2  ├─▶│  …  ├─▶│  n  ├─▶
--    └─────┘  └─────┘  └─────┘  └─────┘

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.HashCores.Composition.Pipelined (
  Pipelined(..),
  )
  where

import           Data.Kind                         (Type)
import           Data.Proxy
import           Data.Singletons.Prelude           (type (@@), Apply, TyFun)

import           Clash.Prelude

import           Clash.HashCores.Class.Composition
import           Clash.HashCores.Class.Iterable


-- | Pipeline a circuit
--
data Pipelined = Pipelined deriving Show

data PipelineStep :: Domain -> Nat -> Nat -> Type -> TyFun Nat Type -> Type
type instance Apply (PipelineStep domain t0 delay s) i
  = DSignal domain t0 s
  -> DSignal domain (t0+(delay*(1+i))) s

instance Default Pipelined where
  def = Pipelined

instance (KnownNat d, 1 <= d) => Composition Pipelined 'Always x d where
  indexedCompose :: forall domain gated synchronous
                           reference
                           iterable _i _o r d' .
      ( HiddenClockReset domain gated synchronous
      , Iterable iterable _i x _o r d'
      , d ~ (r*d')
      )
      => Pipelined
      -> iterable
      -- | In signal
      -> DSignal domain reference (x, ())
      -- | Out signal
      -> ( DSignal domain (reference + d) x
         , DSignal domain  reference      ())

  indexedCompose Pipelined iterable (fmap fst->input) = (,pure ()) $
        dfold (Proxy @(PipelineStep domain reference _ x))
          pipe
          (oneStep iterable 0)
          (reverse . drop d1 . indices $ SNat @(1+(r-1)))
      $ input
    where
      pipe :: SNat l -> Index r
        -> PipelineStep domain reference d' x @@ l
        -> PipelineStep domain reference d' x @@ (l+1)
      pipe _ i f' = oneStep iterable (pure i) . f'
  -- {-# NOINLINE indexedCompose #-}
