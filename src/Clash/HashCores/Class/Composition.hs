{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}

module Clash.HashCores.Class.Composition (
  Input(..),
  Composition,
  indexedCompose,
  IsValid,
  IsReady,
  )
  where

import           Clash.HashCores.Class.Iterable
import           Clash.Prelude

-- | Output is always synced via static type-level timing.
-- Input is either always valid or synced with DataFlow semantics.
data Input
  = Always            -- ^ Input is always valid
  | ValidReadyFlagged -- ^ Input follows dataflow semantics

type IsValid = Bool
type IsReady = Bool

type family Incoming (inputSemantics :: Input) (a :: *) where
  Incoming 'Always a = a
  Incoming 'ValidReadyFlagged a = (a, IsValid)

type family Outgoing (inputSemantics :: Input) (a :: *) where
  Outgoing 'Always a = ()
  Outgoing 'ValidReadyFlagged a = IsReady

class (Iterable iterable _i s _o r d)
      => Composition x
                     iterable
                     (inputSemantics :: Input)
                     _i s _o r d | x -> inputSemantics
  where
    indexedCompose ::
      ( HiddenClockReset domain gated synchronous
      , KnownNat reference )
      => x
      -> iterable

      -- | In signal
      -> DSignal domain reference (Incoming inputSemantics s)

      -- | Out signal
      -> ( DSignal domain  reference        (Outgoing inputSemantics s)
         , DSignal domain (reference + r*d)                          s  )
