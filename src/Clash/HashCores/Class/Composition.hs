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

type family Incoming (inputSemantics :: Input) :: Type where
  Incoming 'Always = ()
  Incoming 'ValidReadyFlagged = Bool -- Is valid

type family Outgoing (inputSemantics :: Input) :: Type where
  Outgoing 'Always = ()
  Outgoing 'ValidReadyFlagged = Bool -- Is ready

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
      ( HiddenClockResetEnable domain
      , Iterable iterable _i x _o r d
      , KnownNat reference
      , delay ~ (r*d)
      )
      => composition
      -> iterable

      -- | In signal
      -> DSignal domain reference (x, Incoming inputSemantics)

      -- | Out signal
      -> ( DSignal domain (reference + delay)  x
         , DSignal domain  reference          (Outgoing inputSemantics))

-- | Easy recovery valid signaling from timing.
isOutputValid :: forall domain
                        reference
                        composition inputSemantics x d.
  ( HiddenClockResetEnable domain
  , Composition composition inputSemantics x d
  , KnownNat reference
  , KnownNat d)
  => composition
  -> DSignal domain  reference      Bool -- ^ On accepted input: valid == ready == true
  -> DSignal domain (reference + d) Bool -- ^ On output
isOutputValid _ = delayN (SNat @d) (errorX "isOutputValid value undefined due to reset")
