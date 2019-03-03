{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoStarIsType           #-}
{-# LANGUAGE ScopedTypeVariables    #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module Clash.HashCores.Class.Paddable (
  Paddable,
  oneBlockPadI,
  oneBlockPadBytes,
  pad,
  )
  where

import qualified Data.ByteString as B

import           Clash.Prelude


-- | Padding for a single input block. Multiple block padding not handled right
-- now.
--
-- Given some 'BitVector' of length @n@, padI will give you a 'BitVector' of
-- length @osize@. 'padI' and 'padBytes' are both intended to be synthesizable.
class Paddable x (isize :: Nat) (osize :: Nat)
  | x -> isize, x -> osize where
    -- | Pad a single 'BitVector' length taken from @n@. (Implies length is static)
    oneBlockPadI
      :: ( KnownNat n
         , n <= isize )
      => x -> BitVector n -> BitVector osize

    -- | Pad with length taken from integer parameter. (Implies length is
    -- dynamic)
    oneBlockPadBytes
      :: ( KnownNat n
         , n <= isize
         , (Mod n 8) ~ 0 )
      => x -> BitVector n -> Integer -> BitVector osize

-- * Non-synthesizable convenience functions

fromBytes :: B.ByteString -> Integer
fromBytes = B.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b

pad :: forall x i o .
    ( Paddable x i o
    , KnownNat i
    , KnownNat o
    , ((Div i 8) * 8) <= i        -- meh
    , (Mod ((Div i 8) * 8) 8) ~ 0 --
    )
    => x -> B.ByteString -> BitVector o
pad x block = oneBlockPadBytes x d s
  where
    s = toInteger $ B.length block
    d = fromInteger (fromBytes block) :: BitVector ((Div i 8) * 8)
