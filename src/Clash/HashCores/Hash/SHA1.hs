-- | SHA-1 https://en.wikipedia.org/wiki/SHA-2

{-# LANGUAGE BinaryLiterals            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoStarIsType              #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Clash.HashCores.Hash.SHA1
  (
  SHA1(..),
  SomeSHA1(..),
  someSHA1,
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

-- SHA-1 initial state
initial :: Vec 5 Word32
initial = 0x67452301 :>
          0xEFCDAB89 :>
          0x98BADCFE :>
          0x10325476 :>
          0xC3D2E1F0 :> Nil

-- | SHA-1 datatype
data SHA1 :: Nat
          -> Nat
          -> Type where
  SHA1 :: ( KnownNat adderDelay
          , KnownNat finalDelay)
         => SHA1 adderDelay finalDelay

instance (KnownNat adderDelay, KnownNat finalDelay)
  => Show (SHA1 adderDelay finalDelay) where
    show _ = "SHA-1 " P.++
             show (SNat @adderDelay) P.++ " " P.++
             show (SNat @finalDelay)

instance (KnownNat a, KnownNat b) => Default (SHA1 a b) where
  def = SHA1 @a @b

instance ( KnownNat adderDelay
         , KnownNat finalDelay
         , KnownNat totalDelay
         , totalDelay ~ ((2*adderDelay) + finalDelay + 1)
         , 1 <= totalDelay)
         => Iterable
            (SHA1 adderDelay finalDelay)
            (BitVector 512)               -- Input
            (Vec 16 Word32, Vec 5 Word32) -- State
            (BitVector 160)               -- Output
            80                            -- Rounds
            totalDelay                    -- Delay
            where

  preIteration  _ = (, initial) . unpack
  postIteration _ = pack . zipWith (+) initial . snd

  oneStep _sha i s = delayN (SNat @finalDelay) undef $ bundle (schedule', state')
    where
      (schedule, state) = unbundle s :: 
            (DSignal _ _ (Vec 16 Word32), DSignal _ _ (Vec 5 Word32))

      undef = errorX "internal SHA1 value undefined due to reset"

      (f,k) = unbundle $ (\(b:>c:>d:>Nil) -> \case
        x | x < 20 ->
            ( (b .&. c) .|. ((complement b) .&. d)
            , 0x5A827999 )
        x | 20 <= x && x < 40 ->
            ( b `xor` c `xor` d
            , 0x6ED9EBA1 )
        x | 40 <= x && x < 60 ->
            ( (b .&. c) .|. (b .&. d) .|. (c .&. d) 
            , 0x8F1BBCDC )
        x | 60 <= x ->
            ( b `xor` c `xor` d
            , 0xCA62C1D6 ))
        <$> (select d1 d1 d3 <$> state) <*> i

      scheduling :: Vec 16 Word32 -> (Word32, Word32)
      scheduling (w0:>_:>w2:>_:>_:>_:>_:>_:>
                  w8:>_:>_ :>_:>_:>w13:>_:>_:>Nil) =
        (w0, rotateL (w0`xor`w2`xor`w8`xor`w13) 1)

      (w0', wn) = unbundle $ scheduling <$> schedule

      state' =
        let (a:>b:>c:>d:>e:>Nil) = unbundle state
            a' = delayN d1 undef $ (delayI undef w0') + delayedFold (SNat @adderDelay) undef (+)
                 ((flip rotateL 5 <$> a) :> f :> e :> k :> Nil)
            c' = delayI undef (flip rotateL 30 <$> b)
        in bundle (a':>delayI undef a:>c':>delayI undef c:>delayI undef d:>Nil)

      schedule' = (<<+) <$> delayI undef schedule <*> delayI undef wn

  {-# NOINLINE oneStep #-}

instance Paddable (SHA1 x y) 447 512 where
    oneBlockPadI
      :: forall n. (KnownNat n, n <= 447)
      => (SHA1 x y) -> BitVector n -> BitVector 512
    oneBlockPadI _ msg =
      let extended = msg ++# (1 :: BitVector 1)
                         ++# (0 :: BitVector (447 - n)) :: BitVector 448
      in pack $ map v2bv (unconcat d32 $ bv2v extended)
                ++ (0 :> fromInteger (natVal msg) :> Nil)

    oneBlockPadBytes
      :: forall n. (KnownNat n , n <= 447)
      => (SHA1 x y) -> BitVector n -> Integer -> BitVector 512
    oneBlockPadBytes _ m bytes =
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
data SomeSHA1
  = forall a b .
    (KnownNat a, KnownNat b)
    => SomeSHA1 (SHA1 a b)

instance Show SomeSHA1 where
  show (SomeSHA1 sha1) = show sha1

-- | 'SomeSHA1' from 'SomeNat's
someSHA1 :: SomeNat -> SomeNat -> SomeSHA1
someSHA1 (SomeNat a) (SomeNat b) = someSHA1' a b
  where
    someSHA1' :: forall a b. (KnownNat a, KnownNat b)
                => Proxy a -> Proxy b -> SomeSHA1
    someSHA1' _ _ = SomeSHA1 (SHA1 @a @b)
