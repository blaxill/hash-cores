{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Clash.HashCores.Class.Composition (
  Composition(..),
  )
  where

import           Clash.Prelude

-- |
class Composition x where
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
    -> DSignal domain t0 a                -- In
    -> DSignal domain t0 Bool             -- Valid
    -> ( DSignal domain (t0+(i*delay)) a  -- Out
       , DSignal domain t0 Bool)          -- Ready
