-- | Extra utility functions

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Clash.HashCores.Utils where

import           Clash.Prelude
import qualified Prelude             as P

import           Data.Char           (ord)
import           Language.Haskell.TH (Exp, ExpQ, Q, TExp, TypeQ)

import           Data.Coerce         (coerce)
import           Data.Maybe          (fromMaybe)

-- | ASCII literal to 'Vec' of 8 bit 'BitVector's.
asciiLit :: String -> ExpQ
asciiLit []     = [| Nil |]
asciiLit (x:xs) = [| (toEnum (ord x) :: BitVector 8) :> $(asciiLit xs) |]

-- TODO: RAM instead of rotating vector
-- printX $ sampleN 12 $ (rotatingCounters d3 SatBound (fromList (Just (0::Unsigned 2): Nothing : Just 0 : Prelude.repeat Nothing)))
rotatingCounters :: forall domain gated synchronous n a d.
                 ( HiddenClockReset domain gated synchronous
                 , SaturatingNum a, Undefined a, KnownNat n
                 , Show a
                 )
                 => SNat (n + 1)
                 -> SaturationMode
                 -> Signal domain (Maybe a) -- ^ Reset current head at time t
                 -> Signal domain a -- ^ Value of counter reset at time t,t+s,t+2s...
rotatingCounters _s saturationMode reset = fromMaybe <$> fmap head counters <*> reset
  where
    setHead (Just v) (_:>xs) = v :> xs
    setHead _ xs             = xs

    placedCounters = setHead <$> reset <*> counters

    inc (x:>xs) =  satAdd saturationMode x 1 :> xs
    rot = flip rotateLeftS d1

    counters :: Signal domain (Vec (n + 1) a)
    counters = register (deepErrorX "Undefined counter") $ rot . inc <$> placedCounters

rotatingCountersLE :: forall domain gated synchronous n a d.
                   ( HiddenClockReset domain gated synchronous
                   , SaturatingNum a, Undefined a, KnownNat n
                   , Show a
                   , 1 <= n
                   )
                 => SNat n
                 -> SaturationMode
                 -> Signal domain (Maybe a)
                 -> Signal domain a
rotatingCountersLE n s v = leToPlus @1 @n $ rotatingCounters n s v

