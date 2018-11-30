-- | SHA-256 core https://en.wikipedia.org/wiki/SHA-2

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module Clash.HashCores.Sha256 (
  State,
  Hashed,
  Message,
  Schedule,

  initial,
  initialize,
  kRom,
  compression,
  compressionTopLevel,
  finalize,

  preprocessI,
  preprocess
  )
  where

import           Clash.HashCores.Utils       (rotatingCountersLE)
import           Clash.Prelude               hiding (bundle, unbundle)
import qualified Clash.Prelude               as P (bundle, unbundle)
import           Clash.Signal.Delayed.Bundle
import           Clash.Sized.BitVector       ((++#))

import qualified Data.Char                   as C (ord)
import           Data.Functor.Compose
import qualified Prelude                     as P

import           Data.Proxy
import           Data.Singletons.Prelude     (type (@@), Apply, TyFun)

import           Control.Lens.Operators      ((<&>))
import           Data.Bifunctor              (first)
import           Data.Maybe                  (fromMaybe, isJust)

type Word32 = BitVector 32
-- | Internal SHA-256 state
type State = Vec 8 Word32
-- | Output hash value
type Hashed = Vec 8 Word32
-- | A single message block
type Message = Vec 16 Word32
-- | Internal SHA-256 message scheduling array
type Schedule = Vec 16 Word32

-- | SHA-256 initial state
initial :: State
initial = 0x6a09e667 :>
          0xbb67ae85 :>
          0x3c6ef372 :>
          0xa54ff53a :>
          0x510e527f :>
          0x9b05688c :>
          0x1f83d9ab :>
          0x5be0cd19 :> Nil

initialize :: Message -> (Message, State)
initialize = (, initial)

-- | SHA-256 round constants
kRom :: Enum addr => addr -> BitVector 32
kRom = asyncRom $(listToVecTH [
      0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
      0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
      0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
      0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
      0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
      0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
      0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
      0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
      0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
      0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
      0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
      0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
      0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
      0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
      0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
      0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
      :: BitVector 32])

-- | A pipelined component that performs the SHA-256 mixing and scheduling
-- step in combination.
compression :: forall domain gated synchronous d adderTreeDelay finalDelay .
      ( HiddenClockReset domain gated synchronous
      , KnownNat adderTreeDelay
      , KnownNat finalDelay)
      => SNat adderTreeDelay                  -- ^ Configurable delay applied after each addition
      -> SNat finalDelay                      -- ^ Final delay applied before function return
      -> DSignal domain d (Index 64) --Word32              -- ^ SHA-256 K constant for this round
      -> DSignal domain d (Schedule, State)   -- ^ Bundled schedule and state
      -> DSignal domain
        (d + (2*adderTreeDelay) + finalDelay)
        (Schedule, State)                     -- ^ Output schedule and state
compression adderTreeDelay finalDelay i scheduleWithState =
    delayN finalDelay $ bundle (schedule', state')
  where
    (schedule, state) = unbundle scheduleWithState
    k = kRom <$> i

    -- * Combinatorial and logically independent steps

    mixing :: State -> Vec 4 Word32
    mixing (a:>b:>c:>d:>e:>f:>g:>h:>Nil) =
      let s1  = rotateR e 6 `xor` rotateR e 11 `xor` rotateR e 25
          s0  = rotateR a 2 `xor` rotateR a 13 `xor` rotateR a 22
          ch  = (e .&. f) `xor` (complement e .&. g)
          maj = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)
       in s1 :> s0 :> ch :> maj :> Nil

    scheduling :: Schedule -> Vec 4 Word32
    scheduling (w0:>w1:>_:>_:>_:>_:>_  :>_:>
                _ :>w9:>_:>_:>_:>_:>w14:>_:>Nil) =
      let s0  = rotateR w1  7  `xor` rotateR w1  18 `xor` shiftR w1  3
          s1  = rotateR w14 17 `xor` rotateR w14 19 `xor` shiftR w14 10
       in w9 :> w0 :> s0 :> s1 :> Nil

    (s1 :> s0 :> ch :> maj :> Nil) = unbundle (mixing <$> state)

    stateIx ix = (!!) <$> state <*> ix

    -- * Combining the values from above, while applying delays after additions.
    -- Both adder trees are 4 element vectors, so the number of delays required
    -- is log2(4)=2, hence the factor of 2 in the output type signature.

    temp1 = delayedFold adderTreeDelay (+) (stateIx 7 :> s1 :> ch :> k + fmap head schedule :> Nil)
    temp2 = delayI (s0 + maj)

    state' = let (a:>b:>c:>d:>e:>f:>g:>_:>Nil) = unbundle (delayI state)
                 a' = temp1+temp2
                 e' = delayI (stateIx 3) + temp1
              in bundle (a':>a:>b:>c:>e':>e:>f:>g:>Nil)

    schedule' = (<<+)
               <$> delayI schedule
               <*> delayedFold adderTreeDelay (+) (unbundle $ scheduling <$> schedule)

-- | A top level version of the step function, intended for benchmarking and debugging.
compressionTopLevel :: SystemClockReset
             => Signal System (Index 64)
             -> Signal System (Schedule, State)
             -> Signal System (Schedule, State)
compressionTopLevel i s = toSignal (compression d1 d0 i' s')
  where
    i' = fromSignal i
    s' = fromSignal s
{-# ANN compressionTopLevel (defSyn "compression") #-}
{-# NOINLINE compressionTopLevel #-}

-- | SHA-256 round finalizer.
finalize :: (Message, State) -> State
finalize = zipWith (+) initial . snd

-- | Preprocess an *n* <= 447 bit 'BitVector' into a single SHA-256 message
-- block by performing the SHA-256 preprocessing step.
preprocessI :: (KnownNat n, n <= 447) => BitVector n -> Vec 16 (BitVector 32)
preprocessI m =
  let extended = resize (m ++# (1 :: BitVector 1)) :: BitVector 448
      shifted  = shiftL extended (447 - fromInteger (natVal m))
  in map v2bv (unconcat d32 $ bv2v shifted) ++ (0 :> fromInteger (natVal m) :> Nil)

-- | Turn a 'String' with length < 56 into a single SHA-256 message block by
-- performing the SHA-256 preprocessing step. (Non synthesizable)
preprocess :: String -> Vec 16 (BitVector 32)
preprocess m
  | P.length m > 55 = errorX "Can't turn strings with len > 55 into a single message block"
  | otherwise = map v2bv . unconcatI . bv2v $
                writeLen $
                flip shiftL (512-(8*(1+P.length m))) $
                (.|.) 0x80 . next $
                P.foldl (\bv c -> next bv .|. cToBV c) 0 m
    where
      cToBV c = toEnum (C.ord c) :: BitVector 512
      next bv = shiftL bv 8
      writeLen bv = bv .|. (8 * toEnum (P.length m))
