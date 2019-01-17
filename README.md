# dsignal-hash-cores

An experimental Clash based hash core library testing type level parametric hash core design.

First stops:

- [HashCores/Class/MerkleDamgard.hs](https://github.com/blaxill/dsignal-hash-cores/blob/master/src/Clash/HashCores/Class/MerkleDamgard.hs)
- [HashCores/Hash/SHA256.hs](https://github.com/blaxill/dsignal-hash-cores/blob/master/src/Clash/HashCores/Hash/SHA256.hs)
- [HashCores/Composition/Pipelined.hs](https://github.com/blaxill/dsignal-hash-cores/blob/master/src/Clash/HashCores/Composition/Pipelined.hs)

### Usage

Compose a chosen hash function with a chosen composition strategy. Check `examples/TopEntities.hs` for simple examples:

~~~
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
~~~

1. Run the Clash compiler with something like the following:

  cabal new-run clashi -- -i./dsignal-hash-cores/src/ -fclash-spec-limit=128 -fclash-inline-limit=128 -outputdir output

2. Then from inside clashi

  :l examples/TopEntities.hs

3. Then compile into verilog

  :verilog

### Testing

```bash
> cabal new-run dsignal-hash-cores-test
All tests
  PipelinedSHA256
    Properties
      (checked by QuickCheck)
        \sha x -> nativeSHA256 x == hashCoreSHA256 sha x:                  OK (72.19s
          +++ OK, passed 100 tests.
      (checked by SmallCheck)
        \sha -> sha 'The quick brown fox jumps over the lazy dog' == <..>: OK (9.68s)
          36 tests completed (but 1 did not meet the condition)
  Tests
    Properties
      (checked by QuickCheck)
        \x -> nativeSHA256 x == hashCoreSHA256 x:                          OK (1.21s)
          +++ OK, passed 100 tests.
    Unit tests
      Hash 'The quick brown fox jumps over the lazy dog' == <..>:          OK (0.01s)

All 4 tests passed (72.19s)
```

