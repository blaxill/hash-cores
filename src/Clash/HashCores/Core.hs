{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module Clash.HashCores.Core (
  Core(..),
  -- hashCore,
  singleBlockPipe,
  )
  where

import           Clash.HashCores.Class.Composition
import           Clash.HashCores.Class.Iterable
import           Clash.HashCores.Compositions        (Pipelined)
import           Clash.Prelude

data Core
  (composition :: *)
  (iterable :: *)
  (inputSemantics :: Input)
  i s o
  (r :: Nat)
  (d :: Nat)
  where
    (:.) :: (Composition composition iterable inputSemantics i s o r d)
         => composition
         -> iterable
         -> Core composition iterable inputSemantics i s o r d

-- | Construct a core by its type
instance ( Default composition
         , Default iterable
         , Composition composition iterable inputSemantics i s o r d)
         => Default (Core composition iterable inputSemantics i s o r d) where
  def = def :. def

singleBlockPipe ::
     ( HiddenClockReset domain gated synchronous
     , Iterable iterable i s o r d
     , KnownNat reference
     )
     => Core Pipelined iterable 'Always i s o r d
     -> DSignal domain reference i
     -> DSignal domain (reference+r*d) o
singleBlockPipe (c :. f) =
   fmap (postIteration f) . snd . indexedCompose c f . fmap (preIteration f)
