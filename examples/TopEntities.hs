{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- 1. Run the Clash compiler with something like the following:
--
-- cabal new-run clashi -- -i./hash-cores/src/ -fclash-spec-limit=128 -fclash-inline-limit=128 -outputdir output
--
-- 2. Then from inside clashi
--
-- :l examples/TopEntities.hs
--
-- 3. Then compile into verilog
--
-- :verilog

import           Clash.Prelude

import           Clash.HashCores.Class.Circuit
import           Clash.HashCores.Compositions
import           Clash.HashCores.Cores
import           Clash.HashCores.Hash.SHA256


systemClockSHAInPlace
  :: Clock System 'Source
  -> Reset System 'Asynchronous
  -> DSignal System 0 (BitVector 512, Bool)
  -> ( DSignal System 192 (BitVector 256)
     , DSignal System 0 Bool)
systemClockSHAInPlace =
  exposeClockReset $ mkCircuit $ SimpleCore InPlace (SHA256 @1 @1)

{-# ANN systemClockSHAInPlace
  (Synthesize
    { t_name   = "shaInPlace"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortProduct ""
                   [ PortName "block"
                   , PortName "valid"
                   ]
                 ]
    , t_output = PortProduct ""
                   [ PortName "hash"
                   , PortName "ready"
                   ]
    }) #-}
{-# NOINLINE systemClockSHAInPlace #-}
-- SHA256 @0 @1
-- LUTs  regs  slices
-- 671   621   232
--
-- SHA256 @1 @1
-- LUTs  regs  slices

systemClockSHAPipelined
  :: Clock System 'Source
  -> Reset System 'Asynchronous
  -> DSignal System 0 (BitVector 512)
  -> DSignal System 64 (BitVector 256)
systemClockSHAPipelined =
  exposeClockReset $ mkCircuitDataOnly $ SimpleCore Pipelined (SHA256 @0 @1)

{-# ANN systemClockSHAPipelined
  (Synthesize
    { t_name   = "shaPipelined"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "block"
                 ]
    , t_output = PortName "hash"
    }) #-}
{-# NOINLINE systemClockSHAPipelined #-}

-- SHA256 @1 @1
-- LUTs  regs  slices
-- 23994 55565 9143
-- 1.328 W
--
-- SHA256 @0 @1
-- LUTs  regs  slices
-- 25418 22426 6780
-- 1.487 W
