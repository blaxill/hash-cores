{-# LANGUAGE BinaryLiterals            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}

module Test.HashCores.SHA256 (
  SingleBlock(..),
  nativeSHA256,
  hashCoreSHA256,
  tests,
  ) where

import           Clash.Prelude                       as Clash hiding ((++))
import           Prelude                             ((++))
import qualified Prelude                             as P

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck               as QC
import           Test.Tasty.SmallCheck               as SC

import           Clash.HashCores.Class.Iterable
import           Clash.HashCores.Class.Paddable
import           Clash.HashCores.Hash.SHA256

import           Crypto.Hash.SHA256                  as H
import           Data.ByteString                     as B
import qualified Data.ByteString.Char8               as B8
import           Data.ByteString.Lazy                (fromStrict)


newtype SingleBlock = SingleBlock
  { unsingleBlock :: B.ByteString }
    deriving (Eq, Ord, Show, Read)

instance Arbitrary SingleBlock where
  arbitrary = do
    -- 55 * 8 = 440 bits, leaving space for SHA-256 padding
    k <- choose (0, 55)
    SingleBlock . B.pack <$> vectorOf k (choose (0, 255))

  shrink (SingleBlock b) = SingleBlock <$>
    [ B.append a b | (a, b) <- P.zip (B.inits b) (P.tail $ B.tails b) ]

nativeSHA256 :: SingleBlock -> BitVector 256
nativeSHA256 =
  fromInteger .
  B.foldl (\acc c -> shiftL acc 8 + toInteger c) 0 .
  H.hash . unsingleBlock

fromBytes :: ByteString -> Integer
fromBytes = B.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b

hashCoreSHA256 :: SingleBlock -> BitVector 256
hashCoreSHA256 (SingleBlock block) = iterableTester sha
    $ padBytes sha d s
  where
    sha = SHA256 @0 @1
    s = toInteger $ B.length block
    d = fromInteger (fromBytes block) :: BitVector 440

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "Hashes random single block (preprocessBytes)" $
      \x -> nativeSHA256 x == hashCoreSHA256 x
  -- , QC.testProperty "Hashes random single block (preprocessBytes)" $
  --     \x -> nativeSHA256 x == hashCoreSHA2562 x
  ]

unitTests = testGroup "Unit tests"
  [ testCase "Hash 'The quick brown fox jumps over the lazy dog'" $ do
      let tqbf = SingleBlock (B8.pack "The quick brown fox jumps over the lazy dog")

      hashCoreSHA256 tqbf == 0xd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592
        @?= True
  ]

tests :: TestTree
tests = testGroup "SHA-256 primitive" [unitTests, properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]
