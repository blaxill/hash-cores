-- | Extra utility functions

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Clash.HashCores.TH where

import           Clash.Prelude

import           Data.Char           (ord)
import           Language.Haskell.TH (ExpQ)

-- | ASCII literal to 'Vec' of 8 bit 'BitVector's.
asciiLit :: String -> ExpQ
asciiLit []     = [| Nil |]
asciiLit (x:xs) = [| (toEnum (ord x) :: BitVector 8) :> $(asciiLit xs) |]
