{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module Clash.HashCores.Cores (
  pipeline,
  core
  )
  where

import           Clash.Explicit.Testbench
import           Clash.HashCores.Utils       (asciiLit, rotatingCountersLE)
import           Clash.Prelude               hiding (bundle, unbundle)
import           Clash.Signal.Delayed.Bundle
import           Clash.Sized.BitVector       ((++#))

import qualified Data.Char                   as C (ord)
import           Data.Functor.Compose
import qualified Prelude                     as P

import           Data.Functor.Syntax         ((<$$>))
import           Data.Proxy
import           Data.Singletons.Prelude     (type (@@), Apply, TyFun)

import           Control.Lens.Operators      ((<&>))
import           Data.Bifunctor              (first)
import           Data.Maybe                  (fromMaybe, isJust)
import           Numeric                     (showHex)

data Step
  (domain :: Domain)
  (time :: Nat)
  (delta :: Nat)
  (state :: *)
  (f :: TyFun Nat *) :: *
type instance Apply (Step domain t d' s) l
  = DSignal domain t s
  -> DSignal domain (t+(d'*(1+l))) s

-- | A simple pipeline that has @nSteps@ number of step units,
-- giving a throughput of 1 item per cycle at a latency of @d'@ * @nSteps@.
pipeline :: forall domain gated synchronous nSteps state i d d'.
         ( HiddenClockReset domain gated synchronous
         , KnownNat nSteps
         , KnownNat d, KnownNat d')
         => SNat (1 + nSteps)
         -- ^ Number of times the function is replicated
         -> (forall x . DSignal domain x (Index (1 + nSteps))
                     -> DSignal domain x state
                     -> DSignal domain (x+d') state)
         -- ^ Function that is replicated through the pipeline
         -> DSignal domain d state
         -> DSignal domain (d+((1 + nSteps) * d')) state
pipeline nSteps stepper =
    dfold (Proxy @(Step domain d _ state))
          pipe
          (stepper 0)
          (reverse . drop d1 . indices $ SNat @(1 + nSteps))
  where
    pipe :: SNat l -> Index (1+nSteps)
      -> Step domain d d' state @@ l
      -> Step domain d d' state @@ (l+1)
    pipe _ i f = stepper (pure i) . f


core :: forall domain gated synchronous d d' nSteps state .
     ( HiddenClockReset domain gated synchronous
     , KnownNat nSteps
     , KnownNat d, KnownNat d'
     , 1 <= d')
     => SNat (1 + nSteps)
     -> (forall x. DSignal domain x (Index (1 + nSteps))
        -> DSignal domain x state
        -> DSignal domain (x+d') state )
     -> DSignal domain d (Maybe state)
     -> DSignal domain (d+((1 + nSteps) * d')) (Maybe state)
core nSteps stepper input = unsafeFromSignal output
  where
    counters :: Signal domain (Index (3 + nSteps))
    counters = rotatingCountersLE (SNat @d') SatBound (const 0 <$$> toSignal input)

    state :: Signal domain state
    state = toSignal
      (stepper
        (unsafeFromSignal $ resize . min (snatToNum nSteps-1) <$> counters)
        (unsafeFromSignal (fromMaybe <$> state <*> toSignal input)))

    output = (\i s -> if i == snatToNum nSteps
      then Just s
      else Nothing)
      <$> counters <*> state
