# dsignal-hash-cores

An experimental Clash based hash core library testing type level parametric hash core design.

### Usage

Compose a chosen hash function with a chosen composition strategy. For example specifying a fully unrolled/pipelined SHA256, with a delay after each addition and round of its compression function is simple:

~~~Haskell
λ> core = Pipelined :.: SHA256 @1 @1
λ> core
Core (Pipelined) (SHA-256 d1 d1)
λ> singleBlockCore core
singleBlockCore core
  :: (?clk::Clock domain gated, ?rst::Reset domain synchronous) =>
     MaybeDSignal domain t0 (BitVector 512)
     -> (DSignal domain (t0 + 192) (BitVector 256),
         DSignal domain t0 Bool)
~~~

The last invocation `singleBlockCore core` instantiates a core that processes single blocks (i.e. cannot process variable length inputs). We can see that the delays induced by pipelining are represented in the result type (here the delay is `+192`cycles)

### Examples

~~~

systemClockSHAIterated :: SystemClockReset
  => DSignal System 0 (Block (SHA256 0 1))
  -> DSignal System 0 Bool
  -> ( DSignal System 64 (Hash (SHA256 0 1))
     , DSignal System 0 Bool )
systemClockSHAIterated = singleBlockCore (Iterated @1 :.: SHA256 @0 @1)
{-# ANN systemClockSHAIterated (defSyn "systemClockShaIterated") #-}
{-# NOINLINE systemClockSHAIterated #-}

systemClockSHAPipelined :: SystemClockReset
  => DSignal System 0 (Block (SHA256 1 0))
  -> DSignal System 0 Bool
  -> ( DSignal System 64 (Hash (SHA256 1 0))
     , DSignal System 0 Bool )
systemClockSHAPipelined = singleBlockCore (Pipelined :.: SHA256 @0 @1)
{-# ANN systemClockSHAPipelined (defSyn "systemClockShaPipelined") #-}
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

