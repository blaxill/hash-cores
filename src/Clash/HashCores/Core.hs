{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Clash.HashCores.Core (
  Core(..),
  hashCore,
  singleBlockCore,
  )
  where

import           Clash.HashCores.Class.Composition
import           Clash.HashCores.Class.MerkleDamgard
import           Clash.Prelude

import qualified Prelude                             as P

import           Data.Bifunctor                      (first)

data Core (composition:: *) (fn :: *) where
    (:.) :: (Composition composition)
         => composition -> fn -> Core composition fn

instance (Show c, Show f) => Show (Core c f) where
  show (c :. f) = "Core (" P.++ show c P.++ ") (" P.++ show f P.++ ")"

-- | Construct a core by its type
instance ( Default composition
         , Composition composition
         , Default f)
         => Default (Core composition f) where
  def = def :. def

-- | Does not perform initialization/finalization
hashCore ::
         ( HiddenClockReset domain gated synchronous
         , Composition c
         , MerkleDamgard hash
         , KnownNat (CompressionRounds hash)
         , 1 <= CompressionDelay hash
         , 1 <= CompressionRounds hash
         )
         => Core c hash
         -> DSignal domain t0 (State hash)     -- In
         -> DSignal domain t0 Bool             -- Valid
         -> ( DSignal domain (t0+(CompressionRounds hash*CompressionDelay hash)) (State hash)
            , DSignal domain t0 Bool)          -- Ready
hashCore (c :. hash) = indexedCompose c (compression hash)

-- | Performs initialization/finalization, but input can only be 1 block long.
singleBlockCore ::
     ( HiddenClockReset domain gated synchronous
     , Composition composition
     , MerkleDamgard hash
     , KnownNat (CompressionRounds hash)
     , 1 <= CompressionDelay hash
     , 1 <= CompressionRounds hash
     )
     => Core composition hash
     -> DSignal domain t0 (Block hash)     -- In
     -> DSignal domain t0 Bool             -- Valid
     -> ( DSignal domain (t0+(CompressionRounds hash*CompressionDelay hash)) (Hash hash)
        , DSignal domain t0 Bool)
singleBlockCore core@(_ :. hash) block valid =
  let block' = fmap (precompression hash) block
   in first (fmap (postcompression hash))
      $ hashCore core block' valid


