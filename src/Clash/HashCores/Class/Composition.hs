{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE NoStarIsType           #-}
{-# LANGUAGE RankNTypes             #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}

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
-- validity should be solely determined by accepted input timing @d@.
class (KnownNat delay, 1 <= delay)
      => Composition composition
                     (inputSemantics :: Input)
                     x
                     delay | composition -> inputSemantics
  where
    indexedCompose ::
      ( HiddenClockReset domain gated synchronous
      , Iterable iterable _i x _o r d
      , KnownNat reference
      , delay ~ (r*d)
      )
      => composition
      -> iterable

      -- | In signal
      -> DSignal domain reference (Incoming inputSemantics x)

      -- | Out signal
      -> ( DSignal domain  reference          (Outgoing inputSemantics x)
         , DSignal domain (reference + delay)  x                         )

-- | Easy recovery valid signaling from timing.
isOutputValid :: forall domain gated synchronous
                        reference
                        composition inputSemantics x d.
  ( HiddenClockReset domain gated synchronous
  , Composition composition inputSemantics x d
  , KnownNat reference
  , KnownNat d)
  => composition
  -> DSignal domain  reference      Bool -- ^ On accepted input: valid == ready == true
  -> DSignal domain (reference + d) Bool -- ^ On output
isOutputValid _ = delayN (SNat @d) (errorX "isOutputValid value undefined due to reset")
