{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Clash.HashCores.Demo where

import qualified Clash.Explicit.Testbench    as TB
import           Clash.HashCores.Cores       (core, pipeline)
import qualified Clash.HashCores.Sha256      as SHA256
import           Clash.HashCores.Utils       (asciiLit)
import           Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D

import           Data.Functor.Syntax         ((<$$>))
import           Numeric                     (showHex)

import qualified Prelude                     as P

simpleShaPipe :: (SystemClockReset, KnownNat d)
                   => DSignal System d SHA256.Message
                   -> DSignal System (d+128) SHA256.State
simpleShaPipe =
    fmap SHA256.finalize .
    pipeline d64 (SHA256.compression d1 d0) .
    fmap SHA256.initialize

simpleShaCore :: (SystemClockReset, KnownNat d)
              => DSignal System d (Maybe SHA256.Message)
              -> DSignal System (d+64) (Maybe SHA256.State)
simpleShaCore =
    (SHA256.finalize <$$>) .
    core d64 (SHA256.compression d0 d1) .
    (SHA256.initialize <$$>)

simpleShaCore'
  :: Clock  System Source
  -> Reset  System Asynchronous
  -> Signal System (Maybe SHA256.Message)
  -> Signal System (Maybe SHA256.State)
simpleShaCore' = exposeClockReset (toSignal . simpleShaCore . fromSignal)
{-# ANN simpleShaCore' (defSyn "simpleShaCore") #-}
{-# NOINLINE simpleShaCore' #-}

-- * Test bench

-- λ> $(asciiLit "The quick brown fox jumps over the lazy dog")
-- ->
-- <1101_0111_1010_1000_1111_1011_1011_0011,0000_0111_1101_0111_1000_0000_1001_0100,0110_1001_1100_1010_1001_1010_1011_1100,1011_0000_0000_1000_0010_1110_0100_1111,1000_1101_0101_0110_0101_0001_1110_0100,0110_1101_0011_1100_1101_1011_0111_0110,0010_1101_0000_0010_1101_0000_1011_1111,0011_0111_1100_1001_1110_0101_1001_0010>
tqbfString :: Vec 43 (BitVector 8)
tqbfString = 0b01010100:> 0b01101000:> 0b01100101:> 0b00100000:> 0b01110001:> 0b01110101:> 0b01101001:> 0b01100011:> 0b01101011:> 0b00100000:> 0b01100010:> 0b01110010:> 0b01101111:> 0b01110111:> 0b01101110:> 0b00100000:> 0b01100110:> 0b01101111:> 0b01111000:> 0b00100000:> 0b01101010:> 0b01110101:> 0b01101101:> 0b01110000:> 0b01110011:> 0b00100000:> 0b01101111:> 0b01110110:> 0b01100101:> 0b01110010:> 0b00100000:> 0b01110100:> 0b01101000:> 0b01100101:> 0b00100000:> 0b01101100:> 0b01100001:> 0b01111010:> 0b01111001:> 0b00100000:> 0b01100100:> 0b01101111:> 0b01100111:> Nil
-- Empty string
-- ->
-- <1101_1010_0101_0110_1001_1000_1011_1110,0001_0111_1011_1001_1011_0100_0110_1001,0110_0010_0011_0011_0101_0111_1001_1001,0111_0111_1001_1111_1011_1110_1100_1010,1000_1100_1110_0101_1101_0100_1001_0001,1100_0000_1101_0010_0110_0010_0100_0011,1011_1010_1111_1110_1111_1001_1110_1010,0001_1000_0011_0111_1010_1001_1101_1000>
-- emptyString :: Vec _ (BitVector 8)

shaTestVec = SHA256.preprocessI $ concatBitVector# tqbfString

toHex :: Vec 8 (BitVector 32) -> String
toHex x = flip showHex "" $ concatBitVector# x

zeroFirstN
  :: (Num a, HiddenClockReset domain gated synchronous )
  => Int -> Signal domain a -> Signal domain a
zeroFirstN n a = mux en a 0
  where
    en = P.iterate (register False) (pure True) P.!! n

shaTestBench :: Signal System Bool
shaTestBench = done
  where
    testInput      = TB.stimuliGenerator clk rst (Just shaTestVec:>Nil)
    expectedOutput = TB.outputVerifier clk rst
      (replicate d63 Nothing ++
      Just (unpack 0xd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592)
      :> Nil)
    done           = expectedOutput (simpleShaCore' clk rst testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen


-- | Non-synthesizable demo function
--
-- The quick brown fox jumps over the lazy dog
-- ->
-- d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592
--
demo :: IO ()
demo = printX $ P.drop 128 $ sampleN 129 top
  where
    top = withClockReset systemClockGen systemResetGen $
            let msg = register shaTestVec
                        (pure $ repeat 0)
             in fmap toHex $ toSignal (simpleShaPipe (fromSignal msg))

-- | Non-synthesizable demo function
--
-- The quick brown fox jumps over the lazy dog
-- ->
-- d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592
--
demo2 :: IO ()
demo2 = printX $ P.drop 65 $ sampleN 66 top
  -- Delay an extra cycle as sampleN pulses reset on the first cycle
  where
    top = withClockReset systemClockGen systemResetGen $
            let msg = register
                        (Just shaTestVec)
                        (pure Nothing)
             in fmap toHex <$> toSignal (simpleShaCore (fromSignal msg))
