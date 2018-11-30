{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.HashCores.Class.MerkleDamgard
  where

import           Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as B
import qualified Prelude                     as P

import           Control.DeepSeq             (NFData)

type Block x = BitVector (BlockSize x)
type Hash x = BitVector (HashSize x)

-- | A class for representing Merkle-Damgard based hash functions.
-- 'compression' is the function to be iterated by a selected
-- composition strategy.
class MerkleDamgard x where
  type BlockSize x :: Nat
  type HashSize x :: Nat
  type CompressionRounds x :: Nat
  type CompressionDelay x :: Nat

  type State x :: *

  compression :: ( HiddenClockReset domain gated synchronous )
              => x
              -> DSignal domain reference (Index (CompressionRounds x))
                 -- ^ A signal determining which compression round we are performing
              -> DSignal domain reference (State x)
                 -- ^ Some input state signal
              -> DSignal domain (reference+CompressionDelay x) (State x)
                 -- ^ Some output state signal

  precompression :: x
                 -> BitVector (BlockSize x)
                 -> State x

  postcompression :: x
                  -> State x
                  -> BitVector (HashSize x)

-- | Unsynthesizable function for basic testing of hash correctness
--
-- e.g.
--
-- >>> import Clash.HashCores.TH
-- >>> import Clash.HashCores.Hash.SHA256
-- >>> import Numeric (showHex)
-- >>> lazyDog = $(asciiLit "The quick brown fox jumps over the lazy dog")
-- >>> lazyDog' = preprocessI $ concatBitVector# lazyDog
-- >>> hashed = hashTester (SHA256 @0 @0) lazyDog'
-- >>> showHex hashed ""
-- "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
--
hashTester
  :: forall x.
  ( MerkleDamgard x
  , 0 ~ CompressionDelay x
  , 1 <= CompressionRounds x
  , NFData (State x)
  , KnownNat (CompressionRounds x))
  => x
  -> BitVector (BlockSize x)
  -> BitVector (HashSize x)
hashTester x = postcompression x . go 0 . precompression  x
  where
    f ix v = P.head . simulate (toSignal. uncurry (compression x) . B.unbundle . unsafeFromSignal) $ [(ix,v)]
    rounds = snatToNum (SNat @(CompressionRounds x - 1)) :: Index (CompressionRounds x)

    go n
      | n == rounds = f n
      | n /= rounds = go (n+1) . f n
      | otherwise = id
