{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module HashCores.Sha256Spec where

import           Clash.Prelude
import qualified Prelude                as P

import           Test.Hspec
import           Test.QuickCheck

import           Clash.HashCores.Cores
import           Clash.HashCores.Sha256 as SHA256

import           Crypto.Hash.SHA256     as H
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import           Data.Maybe             (fromMaybe)

newtype SingleBlock = SingleBlock B.ByteString
  deriving (Eq, Ord, Show, Read)

shrinks :: B.ByteString -> [B.ByteString]
shrinks bs =
  [ B.append a b | (a, b) <- P.zip (B.inits bs) (P.tail $ B.tails bs) ]

instance Arbitrary SingleBlock where
  arbitrary = do
    k <- choose (0, 55)
    SingleBlock . B.pack <$> vectorOf k (choose (0, 255))
  shrink (SingleBlock b) = SingleBlock <$> shrinks b

-- TODO: reuse Demo.hs code
simpleShaPipe :: (SystemClockReset, KnownNat d)
                   => DSignal System d Message
                   -> DSignal System (d+128) State
simpleShaPipe =
    fmap SHA256.finalize .
    pipeline d64 (compression d1 d0) .
    fmap initialize

simpleShaCore :: (SystemClockReset, KnownNat d)
              => DSignal System d (Maybe Message)
              -> DSignal System (d+64) (Maybe State)
simpleShaCore =
    fmap (fmap SHA256.finalize) .
    core d64 (compression d0 d1) .
    fmap (fmap initialize)

pipeSha256 :: SingleBlock -> Integer
pipeSha256 (SingleBlock s) =
    foldl (\acc c -> shiftL acc 32 + toInteger c) 0 $
    P.last . sampleN 129 $
    withClockReset systemClockGen systemResetGen $
    toSignal $ simpleShaPipe @0 $
    pure $ preprocess (B8.unpack s)

coreSha256 :: SingleBlock -> Integer
coreSha256 (SingleBlock s) =
    foldl (\acc c -> shiftL acc 32 + toInteger c) 0 $
    fromMaybe (repeat 0) $
    P.last . sampleN 66 $
    withClockReset systemClockGen systemResetGen $
    toSignal $ simpleShaCore $
    -- Avoid the reset pulse from sampleN by delaying an extra cycle
    fromSignal $ fromList (Nothing : Just (preprocess (B8.unpack s)) : P.repeat Nothing)

nativeSha256 :: SingleBlock -> Integer
nativeSha256 (SingleBlock s) =
  B.foldl (\acc c -> shiftL acc 8 + toInteger c) 0 $
  H.hash s

spec = do
  describe "SHA-256 pipeline d64 (compression d1 d0)" $ do
      it "hashes empty string" $
        pipeSha256 (SingleBlock $ B.pack []) ==
          0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
      it "hashes 'The quick brown fox jumps over the lazy dog'" $
        pipeSha256 (SingleBlock $ B8.pack
          "The quick brown fox jumps over the lazy dog") ==
          0xd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592
      it "hashes random single blocks" $
        property $ \x -> pipeSha256 x == nativeSha256 x
  describe "SHA-256 core d64 (compression d0 d1)" $ do
      it "hashes empty string" $
        coreSha256 (SingleBlock $ B.pack []) ==
          0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
      it "hashes 'The quick brown fox jumps over the lazy dog'" $
        coreSha256 (SingleBlock $ B8.pack
          "The quick brown fox jumps over the lazy dog") ==
          0xd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592
      it "hashes random single blocks" $
        property $ \x -> coreSha256 x == nativeSha256 x
