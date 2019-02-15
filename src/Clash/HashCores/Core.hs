{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Clash.HashCores.Core (
  Core(..),
  hashCore,
  singleBlockPipe,
  )
  where

import           Clash.HashCores.Class.Composition
import           Clash.HashCores.Class.MerkleDamgard
import           Clash.HashCores.Compositions        (Pipelined)
import           Clash.Prelude

import qualified Prelude                             as P

data Core (composition:: *) (inputSync :: InputSync) (fn :: *) where
    (:.) :: (Composition composition inputSync)
         => composition -> fn -> Core composition inputSync fn

instance (Show c, Show f) => Show (Core c s f) where
  show (c :. f) = "Core (" P.++ show c P.++ ") (" P.++ show f P.++ ")"

-- | Construct a core by its type
instance ( Default composition
         , Composition composition inputSync
         , Default f)
         => Default (Core composition inputSync f) where
  def = def :. def

-- | Does not perform initialization/finalization
hashCore ::
         ( HiddenClockReset domain gated synchronous
         , Composition c s
         , MerkleDamgard hash
         , KnownNat (CompressionRounds hash)
         , 1 <= CompressionDelay hash
         , 1 <= CompressionRounds hash
         )
         => Core c s hash
         -> DSignal domain reference (State hash)
         -> SyncFn s domain reference
            (CompressionRounds hash*CompressionDelay hash) (State hash)
hashCore (c :. hash) = indexedCompose c (compression hash)

-- | Performs initialization/finalization, but input can only be 1 block long.
singleBlockPipe ::
     ( HiddenClockReset domain gated synchronous
     , MerkleDamgard hash
     , KnownNat (CompressionRounds hash)
     , 1 <= CompressionDelay hash
     , 1 <= CompressionRounds hash
     )
     => Core Pipelined 'Always hash
     -> DSignal domain reference (Block hash)
     -> DSignal domain (reference+(CompressionRounds hash*CompressionDelay hash)) (Hash hash)
singleBlockPipe core@(_ :. hash) =
   fmap (postcompression hash) . hashCore core . fmap (precompression hash)
