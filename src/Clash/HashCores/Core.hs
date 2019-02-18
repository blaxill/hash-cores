{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE StandaloneDeriving   #-}

-- To allow unused constraint parameters that are specified
-- by a functional dependency
{-# LANGUAGE UndecidableInstances #-}

module Clash.HashCores.Core (
  Core(..),
  singleBlockPipe,
  )
  where

import           Clash.HashCores.Class.Composition
import           Clash.HashCores.Class.Iterable
import           Clash.HashCores.Compositions      (Pipelined)
import           Clash.Prelude

-- | Wrap a composition and iterable with some input semantics and a known
-- input to output delay.
data Core
  (composition :: *)
  (iterable :: *)
  (inputSemantics :: Input)
  i o
  (d :: Nat)
  where
    -- | Interal state parameter @s@ is hidden, and rounds @r@ delay @d@ are
    -- combined for their total delay.
    (:.) :: forall composition iterable inputSemantics i s o r d.
         ( Composition composition iterable inputSemantics i s o r d)
         => composition
         -> iterable
         -> Core composition iterable inputSemantics i o (r*d)

-- | Construct a core by its type
instance ( Default composition
         , Default iterable
         , Composition composition iterable inputSemantics i s o r d
         , d' ~ (r*d)
         )
         => Default (Core composition iterable inputSemantics i o d') where
  def = def :. def

-- | Construct a core by its type
deriving instance
         ( Show composition
         , Show iterable
         , Composition composition iterable inputSemantics i s o r d
         , d' ~ (r*d)
         )
         => Show (Core composition iterable inputSemantics i o d')

-- |
singleBlockPipe ::
     ( HiddenClockReset domain gated synchronous
     , Iterable iterable i s o r d
     , KnownNat reference
     )
     => Core Pipelined iterable 'Always i o (r*d)
     -> DSignal domain reference i
     -> DSignal domain (reference+r*d) o
singleBlockPipe (c :. f) =
   fmap (postIteration f) . snd . indexedCompose c f . fmap (preIteration f)
