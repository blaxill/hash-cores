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

import           Clash.HashCores.Compositions
import           Clash.HashCores.Core
import           Clash.HashCores.Hash.SHA256

import           Clash.Prelude

systemClockSHAPipelined
  :: Clock System 'Source
  -> Reset System 'Asynchronous
  -> DSignal System 0 (BitVector 512)
  -> DSignal System 192 (BitVector 256)
systemClockSHAPipelined = exposeClockReset (singleBlockPipe (Pipelined :. SHA256 @1 @1))
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
