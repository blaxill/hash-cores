{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE NoStarIsType           #-}
{-# LANGUAGE RankNTypes             #-}

module Clash.HashCores.Class.Composition (
  Input(..),
  Composition,
  indexedCompose,

  Incoming,
  Outgoing,

  isOutputValid,

  IsValid,
  IsReady,
  )
  where

import           Data.Kind                      (Type)

import           Clash.HashCores.Class.Iterable
import           Clash.Prelude

-- | Output is always synced via static type-level timing.
-- Input is either always valid or synced with DataFlow semantics.
data Input
  = Always            -- ^ Input is always valid
  | ValidReadyFlagged -- ^ Input follows dataflow semantics

type IsValid = Bool
type IsReady = Bool

type family Incoming (inputSemantics :: Input) (a :: Type) where
  Incoming 'Always a = a
  Incoming 'ValidReadyFlagged a = (a, IsValid)

type family Outgoing (inputSemantics :: Input) (a :: Type) where
  Outgoing 'Always a = ()
  Outgoing 'ValidReadyFlagged a = IsReady

-- | Compose an 'iterable' function with some 'composition'. This is analogous to
-- "fold iterable [0..r]", but also determines the circuit layout. Output
-- validity should be solely determined when input is accepted and the timing
-- @r@*@d@.
class (Iterable iterable _i s _o r d)
      => Composition composition
                     iterable
                     (inputSemantics :: Input)
                     _i s _o r d | composition -> inputSemantics
  where
    indexedCompose ::
      ( HiddenClockReset domain gated synchronous
      , KnownNat reference )
      => composition
      -> iterable

      -- | In signal
      -> DSignal domain reference (Incoming inputSemantics s)

      -- | Out signal
      -> ( DSignal domain  reference        (Outgoing inputSemantics s)
         , DSignal domain (reference + r*d)                          s )

-- | Easy recovery valid signaling from timing.
isOutputValid :: forall domain gated synchronous
                        reference
                        composition iterable inputSemantics _i s _o r d.
  ( HiddenClockReset domain gated synchronous
  , Composition composition iterable inputSemantics _i s _o r d
  , KnownNat reference
  , KnownNat (r*d))
  => composition
  -> iterable
  -> DSignal domain  reference        Bool -- ^ On accepted input: valid == ready == true
  -> DSignal domain (reference + r*d) Bool -- ^ On output
isOutputValid _ _ = delayN (SNat @(r*d))
