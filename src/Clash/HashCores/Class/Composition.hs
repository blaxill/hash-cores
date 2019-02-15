{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}

module Clash.HashCores.Class.Composition (
  Composition(..),
  InputSync(..),
  SyncFn,
  )
  where

import           Clash.Prelude

-- | Output is always synced via static type-level timing.
-- Input is either synced via type-level timing or DataFlow semantics.
data InputSync
  = Always -- ^ Input is always valid
  | Flow  -- ^ Input follows dataflow semantics

-- | Simple closed type family selection between Always & Flow synchronizations
type family SyncFn
  (inputSync :: InputSync)
  (domain :: Domain)
  (reference :: Nat)
  (delay :: Nat)
  (a :: *)
  where
    -- | Synchronization purely on delay annotation
    SyncFn 'Always domain reference delay a
      = DSignal domain (reference+delay) a -- Out

    -- | Synchronization with DataFlow semantics
    SyncFn 'Flow domain reference delay a
      = DSignal domain reference Bool          -- In valid
      -> ( DSignal domain (reference+delay) a  -- Out
         , DSignal domain reference Bool)      -- In ready

-- |
class Composition x (inputSync :: InputSync) | x -> inputSync where
  indexedCompose ::
    ( HiddenClockReset domain gated synchronous
    , KnownNat i
    , 1 <= i
    , 1 <= delay)
    => x
    -- | Function to compose: index -> state -> state
    -> ( forall t0'. DSignal domain t0' (Index i)
        -> DSignal domain t0' a
        -> DSignal domain (t0'+delay) a
       )
    -- | In signal
    -> DSignal domain reference a
    -- | Synchronization semantics
    -> SyncFn inputSync domain reference (i*delay) a
