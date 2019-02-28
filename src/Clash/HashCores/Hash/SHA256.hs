-- | SHA-256 https://en.wikipedia.org/wiki/SHA-2

{-# LANGUAGE BinaryLiterals            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoStarIsType              #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Clash.HashCores.Hash.SHA256
  (
  SHA256(..),
  SomeSHA256(..),
  someSHA256,
  )
where

import           Data.Kind                      (Type)
import           Data.Proxy                     (Proxy)
import qualified Prelude                        as P

import           Clash.Prelude                  hiding (bundle, unbundle)
import           Clash.Signal.Delayed.Bundle
import           Clash.Sized.BitVector          ((++#))


import           Clash.HashCores.Class.Iterable
import           Clash.HashCores.Class.Paddable

type Word32 = BitVector 32

-- SHA-256 initial state
initial :: Vec 8 Word32
initial = 0x6a09e667 :>
          0xbb67ae85 :>
          0x3c6ef372 :>
          0xa54ff53a :>
          0x510e527f :>
          0x9b05688c :>
          0x1f83d9ab :>
          0x5be0cd19 :> Nil

-- SHA-256 round parameter K table
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

-- | SHA-256 datatype
data SHA256 :: Nat
          -> Nat
          -> Type where
  SHA256 :: ( KnownNat adderDelay
            , KnownNat finalDelay)
         => SHA256 adderDelay finalDelay

instance (KnownNat adderDelay, KnownNat finalDelay)
  => Show (SHA256 adderDelay finalDelay) where
    show _ = "SHA-256 " P.++
             show (SNat @adderDelay) P.++ " " P.++
             show (SNat @finalDelay)

instance (KnownNat a, KnownNat b) => Default (SHA256 a b) where
  def = SHA256 @a @b

instance ( KnownNat adderDelay
         , KnownNat finalDelay
         , KnownNat totalDelay
         , totalDelay ~ ((2*adderDelay) + finalDelay)
         , 1 <= totalDelay)
         => Iterable
            (SHA256 adderDelay finalDelay)
            (BitVector 512)               -- Input
            (Vec 16 Word32, Vec 8 Word32) -- State
            (BitVector 256)               -- Output
            64                            -- Rounds
            totalDelay                    -- Delay
            where

  preIteration  _ = (, initial) . unpack
  postIteration _ = pack . zipWith (+) initial . snd

  oneStep _sha i s = delayN (SNat @finalDelay) $ bundle (schedule', state')
    where
      (schedule, state) = unbundle s
      k = kRom <$> i

      -- * Combinatorial and logically independent steps

      mixing :: Vec 8 Word32 -> Vec 4 Word32
      mixing (a:>b:>c:>_d:>e:>f:>g:>_h:>Nil) =
        let s1  = rotateR e 6 `xor` rotateR e 11 `xor` rotateR e 25
            s0  = rotateR a 2 `xor` rotateR a 13 `xor` rotateR a 22
            ch  = (e .&. f) `xor` (complement e .&. g)
            maj = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)
         in s1 :> s0 :> ch :> maj :> Nil

      scheduling :: Vec 16 Word32 -> Vec 4 Word32
      scheduling (w0:>w1:>_:>_:>_:>_:>_  :>_:>
                  _ :>w9:>_:>_:>_:>_:>w14:>_:>Nil) =
        let s0  = rotateR w1  7  `xor` rotateR w1  18 `xor` shiftR w1  3
            s1  = rotateR w14 17 `xor` rotateR w14 19 `xor` shiftR w14 10
         in w9 :> w0 :> s0 :> s1 :> Nil

      (s1' :> s0' :> ch' :> maj' :> Nil) = unbundle (mixing <$> state)

      -- * Combine the values from above, while applying delays after additions.
      -- Both adder trees are 4 element vectors, so the number of delays required
      -- is log2(4)=2, hence the factor of 2 in the output type signature.

      stateIx :: (KnownNat n, KnownNat n0, (n + (n0+1)) ~ 8)
              => SNat n -> DSignal _ _ Word32
      stateIx ix = fmap (at ix) state

      temp1 = delayedFold (SNat @adderDelay) (+)
              (stateIx d7 :> s1' :> ch' :> k + fmap head schedule :> Nil)

      temp2 = delayI (s0' + maj')

      state' = let (a:>b:>c:>_d:>e:>f:>g:>_:>Nil) = unbundle (delayI state)
                   a' = temp1+temp2
                   e' = delayI (stateIx d3) + temp1
                in bundle (a':>a:>b:>c:>e':>e:>f:>g:>Nil)

      schedule' = (<<+)
                 <$> delayI schedule
                 <*> delayedFold (SNat @adderDelay) (+) (unbundle $ scheduling <$> schedule)

instance Paddable (SHA256 x y) 447 512 where
    padI :: forall n. (KnownNat n, n <= 447)
         => (SHA256 x y) -> BitVector n -> BitVector 512
    padI _ msg =
      let extended = msg ++# (1 :: BitVector 1)
                         ++# (0 :: BitVector (447 - n)) :: BitVector 448
      in pack $ map v2bv (unconcat d32 $ bv2v extended)
                ++ (0 :> fromInteger (natVal msg) :> Nil)

    padBytes :: forall n. (KnownNat n , n <= 447)
             => (SHA256 x y) -> BitVector n -> Integer -> BitVector 512
    padBytes _ m bytes =
      let extended = m ++# (0 :: BitVector (448 - n)) :: BitVector 448
          bytes8 = bytes * 8
          shifted = rotateLeft (unconcatBitVector# extended) (55 - bytes)
          replacedBytes = pack $ izipWith (\i b v ->
                                 case compare (fromIntegral i) b of
                                   LT ->  v
                                   EQ -> 0b10000000
                                   GT -> 0
                                 ) (replicate d56 bytes) shifted
      in pack $ unconcatBitVector# replacedBytes
                ++ ((0 :: BitVector 32) :> fromInteger bytes8 :> Nil)

-- | Convenience existential wrapper
data SomeSHA256
  = forall a b .
    (KnownNat a, KnownNat b)
    => SomeSHA256 (SHA256 a b)

instance Show SomeSHA256 where
  show (SomeSHA256 sha256) = show sha256

-- | 'SomeSHA256' from 'SomeNat's
someSHA256 :: SomeNat -> SomeNat -> SomeSHA256
someSHA256 (SomeNat a) (SomeNat b) = someSHA256' a b
  where
    someSHA256' :: forall a b. (KnownNat a, KnownNat b)
                => Proxy a -> Proxy b -> SomeSHA256
    someSHA256' _ _ = SomeSHA256 (SHA256 @a @b)

