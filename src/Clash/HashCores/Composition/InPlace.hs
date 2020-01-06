--    ┌─────┐
-- ─┬▶│  f  ├┬─▶
--  │ └─────┘│
--  └────────┘

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.HashCores.Composition.InPlace (
  InPlace(..),
  )
  where

import           Clash.Prelude

import           Clash.HashCores.Class.Composition
import           Clash.HashCores.Class.Iterable

-- | A core that iterates in place without explicit storage of state (although
-- implicitly stored in the iterated function), by connecting input to output
-- and tracking iteration indices.
data InPlace = InPlace deriving Show

instance (KnownNat rd, 1 <= rd) => Composition InPlace 'ValidReadyFlagged x rd where
  indexedCompose :: forall domain
                           reference
                           iterable _i _o r d .
      ( HiddenClockResetEnable domain
      , Iterable iterable _i x _o r d
      , rd ~ (r*d)
      )
      => InPlace
      -> iterable
      -- | In signal
      -> DSignal domain reference (x, Bool)
      -- | Out signal
      -> ( DSignal domain (reference + rd) x
         , DSignal domain  reference      Bool)
  -- XXX: Our timing type safety breaks down in here!
  indexedCompose InPlace iterable (unbundle . toSignal -> (input,valid)) =
      ( unsafeFromSignal state
      , unsafeFromSignal ready )
    where
      accept = valid .==. (ready .==. pure True)

      rounds = snatToNum (SNat @r)

      roundIx' :: Signal domain (Index r)
      roundIx' = resize . min (rounds-1) <$> roundIx

      inRound = mux accept 0     roundIx'
      inState = mux accept input state

      state :: Signal domain x
      state = toSignal . uncurry (oneStep iterable) $ (fromSignal inRound, fromSignal inState)

      ready = (>=) rounds <$> roundIx

      --

      ticker :: Signal domain (Index d)
      ticker = register 0 (satAdd SatWrap 1 <$> ticker)

      roundIx :: Signal domain (Index (r + 1))
      roundIx = if natVal (SNat @d) > 1
                then blockRam
                  (replicate (SNat @d) rounds)
                  (satAdd SatWrap 1 <$> ticker) -- (ticker)
                  ((\x y -> Just (x,y)) <$> ticker <*> ((1+). resize <$> inRound))
                else register rounds ((1+). resize <$> inRound)
