{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE StandaloneDeriving   #-}

-- To allow unused constraint parameters that are specified
-- by a functional dependency
{-# LANGUAGE UndecidableInstances #-}

module Clash.HashCores.Core (
  Core(..),
  singleBlockPipe,
  )
  where

import           Data.Kind                         (Type)

import           Clash.HashCores.Class.Composition
import           Clash.HashCores.Class.Iterable
import           Clash.HashCores.Compositions      (Pipelined)
import           Clash.Prelude

-- | Wrap a composition and iterable with some input semantics and a known
-- input to output delay.
data Core
  (composition :: Type)
  (iterable ::Type)
  (inputSemantics :: Input)
  input
  output
  (delay :: Nat)
  where
    -- | Interal state parameter @s@ is hidden, and rounds @r@ delay @d@ are
    -- combined for their total delay.
    (:.) :: forall composition iterable inputSemantics i s o r d.
         ( Composition composition inputSemantics s (r*d)
         , Iterable iterable i s o r d)
         => composition
         -> iterable
         -> Core composition iterable inputSemantics i o (r*d)

-- | Construct a core by its type
instance ( Default composition
         , Default iterable
         , Composition composition inputSemantics s rd
         , Iterable iterable i s o r d
         , rd ~ (r*d)
         )
         => Default (Core composition iterable inputSemantics i o rd) where
  def = def :. def

-- | Show instance for Core
deriving instance
         ( Show composition
         , Show iterable
         , Composition composition inputSemantics s rd
         , Iterable iterable i s o r d
         , rd ~ (r*d)
         )
         => Show (Core composition iterable inputSemantics i o rd)

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
