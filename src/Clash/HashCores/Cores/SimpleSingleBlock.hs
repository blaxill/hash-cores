{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE StandaloneDeriving    #-}

-- To allow unused constraint parameters that are specified
-- by a functional dependency
{-# LANGUAGE UndecidableInstances  #-}

module Clash.HashCores.Cores.SimpleSingleBlock (
  SimpleCore(..),
  -- singleBlockPipe,
  )
  where

import           Data.Bifunctor                    (first)
import           Data.Kind                         (Type)

import           Clash.Prelude

import           Clash.HashCores.Class.Circuit
import           Clash.HashCores.Class.Composition
import           Clash.HashCores.Class.Iterable

-- | Wrap a composition and iterable with some input semantics and a known
-- input to output delay.
data SimpleCore
  (composition :: Type)
  (iterable ::Type)
  (inputSemantics :: Input)
  input
  output
  (delay :: Nat)
  where
    -- | Interal state parameter @s@ is hidden, and rounds @r@ delay @d@ are
    -- combined for their total delay.
    SimpleCore
      :: forall composition iterable inputSemantics i s o r d.
      ( Composition composition inputSemantics s (r*d)
      , Iterable iterable i s o r d)
      => composition
      -> iterable
      -> SimpleCore composition iterable inputSemantics i o (r*d)

-- | Construct a core by its type
instance ( Default composition
         , Default iterable
         , Composition composition inputSemantics s rd
         , Iterable iterable i s o r d
         , rd ~ (r*d)
         )
         => Default (SimpleCore composition iterable inputSemantics i o rd) where
  def = SimpleCore def def

-- | Show instance for SimpleCore
deriving instance
         ( Show composition
         , Show iterable
         , Composition composition inputSemantics s rd
         , Iterable iterable i s o r d
         , rd ~ (r*d)
         )
         => Show (SimpleCore composition iterable inputSemantics i o rd)

instance ( Composition composition inputSemantics s rd
         , Iterable iterable i s o r d
         , (r*d) ~ rd
         )
         => Circuit (SimpleCore composition iterable inputSemantics i o rd)
                    inputSemantics
                    i
                    o
                    rd
         where
  mkCircuit (SimpleCore c f) = first (fmap $ postIteration f)
                             . indexedCompose c f
                             . fmap (first $ preIteration f)
