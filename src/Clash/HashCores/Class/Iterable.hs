{-# LANGUAGE FunctionalDependencies #-}

module Clash.HashCores.Class.Iterable (
  Iterable,
  preIteration,
  postIteration,
  oneStep,
  iterableTester
  )
  where

import qualified Prelude                     as P

import           Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as B

class ( KnownNat rounds
      , KnownNat delay
      , 1 <= rounds
      , 1 <= delay)
      => Iterable x i s o (rounds :: Nat) (delay :: Nat)
         | x -> i, x -> s, x -> o, x -> rounds, x -> delay where

  preIteration :: x -> i -> s
  postIteration :: x -> s -> o

  oneStep :: ( HiddenClockResetEnable domain )
          => x
          -> DSignal domain reference (Index rounds)
             -- ^ Current index
          -> DSignal domain reference s
             -- ^ Some input signal
          -> DSignal domain (reference+delay) s
             -- ^ Some output signal

-- | Non-synthesizable function for basic testing
--
-- e.g.
--
-- >>> import Clash.HashCores.TH
-- >>> import Clash.HashCores.Hash.SHA256
-- >>> import Numeric (showHex)
-- >>> lazyDog = $(asciiLit "The quick brown fox jumps over the lazy dog")
-- >>> lazyDog' = preprocessI $ concatBitVector# lazyDog
-- >>> hashed = iterableTester (SHA256 @0 @1) lazyDog'
-- >>> showHex hashed ""
-- "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
--
iterableTester
  :: forall x i s o r d.
  ( Iterable x i s o r d
  , NFDataX s
  , KnownNat (r-1)
  , 1 ~ d)
  => x
  -> i
  -> o
iterableTester x = postIteration x . go 0 . preIteration  x
  where
    f ix v = P.head . P.drop 1 . simulate @System (toSignal . uncurry (oneStep x) . B.unbundle . unsafeFromSignal) $ P.repeat (ix,v)
    rounds = snatToNum (SNat @(r-1)) :: Index (r)

    go n
      | n == rounds = f n
      | n /= rounds = go (n+1) . f n
      | otherwise = id
