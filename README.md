# hash-cores

An experimental Clash library for building type configured FPGA hash cores with precise types. 

~~~haskell
λ> :t SimpleCore Pipelined (SHA256 @1 @1)
SimpleCore Pipelined (SHA256 @1 @1)
  :: SimpleCore
       Pipelined       -- Composition type
       (SHA256 1 1)    -- SHA256 with register placement parameters
       'Always         -- Input validity semantics
       (BitVector 512) -- Input shape
       (BitVector 256) -- Output shape
       192             -- Input to output cycling timing

λ> :t mkCircuitDataOnly (SimpleCore Pipelined (SHA256 @1 @1))
  mkCircuitDataOnly (SimpleCore Pipelined (SHA256 @1 @1))
    :: (?clk::Clock domain gated,
        ?rst::Reset domain synchronous,
        KnownNat reference)
    => DSignal domain reference (BitVector 512)
    -> DSignal domain (reference + 192) (BitVector 256)
~~~


First stops:

- [src/Clash/HashCores/Class/](https://github.com/blaxill/dsignal-hash-cores/blob/master/src/Clash/HashCores/Class/)
- [src/Clash/HashCores/Hash/](https://github.com/blaxill/dsignal-hash-cores/blob/master/src/Clash/HashCores/Hash/)
- [src/Clash/HashCores/Core/](https://github.com/blaxill/dsignal-hash-cores/blob/master/src/Clash/HashCores/Core/)


### Usage

Compose a chosen hash function with a chosen composition strategy. Check `examples/TopEntities.hs` for simple examples.

1. Run the Clash compiler with something like the following:

  `cabal new-run clashi -- -i./hash-cores/src/ -fclash-spec-limit=500 -fclash-inline-limit=500 -outputdir output`

2. Then from inside clashi

  ~~~
  :l examples/TopEntities.hs
  :verilog
  ~~~


### Testing

```bash
> cabal new-run hash-cores-test
Up to date
Built with thread support: True
Hash core tests
  SHA-256 primitive
    Unit tests
      Hash 'The quick brown fox jumps over the lazy dog':   OK
    Properties
      (checked by QuickCheck)
        Hashes random single block (preprocessBytes):       OK (0.02s)
          +++ OK, passed 4 tests.
  InPlace :. SHA-256 @X @Y
    Properties
      (checked by QuickCheck)
        Hashes random single block:                         OK (0.25s)
          +++ OK, passed 4 tests.
      (checked by SmallCheck)
        Hash 'The quick brown fox jumps over the lazy dog': OK (0.33s)
          16 tests completed (but 1 did not meet the condition)
  Pipelined :. SHA-256 @X @Y
    Properties
      (checked by QuickCheck)
        Hashes random single block:                         OK (5.21s)
          +++ OK, passed 4 tests.
      (checked by SmallCheck)
        Hash 'The quick brown fox jumps over the lazy dog': OK (3.13s)
          16 tests completed (but 1 did not meet the condition)

All 6 tests passed (5.22s)
```

