{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- 1. Run the Clash compiler with something like the following:
--
-- cabal new-run clashi -- -i./dsignal-hash-cores/src/ -fclash-spec-limit=128 -fclash-inline-limit=128 -outputdir output
--
-- 2. Then from inside clashi
--
-- :l examples/TopEntities.hs
--
-- 3. Then compile into verilog
--
-- :verilog

import           Clash.HashCores.Class.MerkleDamgard
import           Clash.HashCores.Compositions
import           Clash.HashCores.Core
import           Clash.HashCores.Hash.SHA256

import           Clash.Prelude

systemClockSHAIterated
  :: Clock System Source
  -> Reset System Asynchronous
  -> DSignal System 0 (Block (SHA256 0 1))
  -> DSignal System 0 Bool
  -> ( DSignal System 64 (Hash (SHA256 0 1))
     , DSignal System 0 Bool )
systemClockSHAIterated = exposeClockReset (singleBlockCore (Iterated @1 :. SHA256 @0 @1))
{-# ANN systemClockSHAIterated
  (Synthesize
    { t_name   = "shaIterated"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "block"
                 , PortName "ready"
                 ]
    , t_output = PortProduct "" [PortName "hash", PortName "valid"]
    }) #-}
{-# NOINLINE systemClockSHAIterated #-}

systemClockSHAPipelined
  :: Clock System Source
  -> Reset System Asynchronous
  -> DSignal System 0 (Block (SHA256 0 1))
  -> DSignal System 0 Bool
  -> ( DSignal System 64 (Hash (SHA256 0 1))
     , DSignal System 0 Bool )
systemClockSHAPipelined = exposeClockReset (singleBlockCore (Pipelined :. SHA256 @0 @1))
{-# ANN systemClockSHAPipelined
  (Synthesize
    { t_name   = "shaPipelined"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "block"
                 , PortName "ready"
                 ]
    , t_output = PortProduct "" [PortName "hash", PortName "valid"]
    }) #-}
{-# NOINLINE systemClockSHAPipelined #-}
