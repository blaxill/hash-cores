# hash-cores

An experimental Clash based hash library testing type level parametric hash core design. Expressive types determine the timing of hash functions:

~~~haskell
type MySHA = Core Pipelined
                  (SHA256 1 1)    -- SHA256 with timing parameters
                  'Always         -- Input validity semantics
                  (BitVector 512) -- Input shape
                  (BitVector 256) -- Output shape
                  192             -- Input to output timing
					
-- Construct value representation either explicitly 
λ> Pipelined :. (SHA256 @1 @1)
(:.) Pipelined SHA-256 d1 d1

-- Or with default instance, via type annotation
λ> def :: MySHA
(:.) Pipelined SHA-256 d1 d1

~~~

Note: The choice of `Pipelined` and `SHA256 @1 @1` fully determines the other parameters:

~~~haskell
λ> type MySHA = Core Pipelined (SHA256 1 1) 'Always (BitVector 512) (BitVector 256) 192
λ> def :: MySHA
(:.) Pipelined SHA-256 d1 d1

λ> type FailMySHA = Core Pipelined (SHA256 1 1) 'Always (BitVector 512) (BitVector 256) 333
λ> def :: FailMySHA

<interactive>:69:1: error:
    • Couldn't match type ‘192’ with ‘333’ arising from a use of ‘def’
    • In the expression: def :: FailMySHA
      In an equation for ‘it’: it = def :: FailMySHA
~~~

First stops:

- [src/Clash/HashCores/Class/*](https://github.com/blaxill/dsignal-hash-cores/blob/master/src/Clash/HashCores/Class/)
- [src/Clash/HashCores/Core.hs](https://github.com/blaxill/dsignal-hash-cores/blob/master/src/Clash/HashCores/Core.hs)
- [src/Clash/HashCores/Hash/Sha.hs](https://github.com/blaxill/dsignal-hash-cores/blob/master/src/Clash/HashCores/Hash/SHA256.hs)


### Usage

Compose a chosen hash function with a chosen composition strategy. Check `examples/TopEntities.hs` for simple examples.

1. Run the Clash compiler with something like the following:

  `cabal new-run clashi -- -i./hash-cores/src/ -fclash-spec-limit=128 -fclash-inline-limit=128 -outputdir output`

2. Then from inside clashi

  ~~~
  :l examples/TopEntities.hs`
  :verilog
  ~~~

### Testing

```bash
> cabal new-run hash-cores-test
Hash core tests
  SHA-256 primitive
    Unit tests
      Hash 'The quick brown fox jumps over the lazy dog':   OK (0.01s)
    Properties
      (checked by QuickCheck)
        Hashes random single block (preprocessBytes):       OK (1.10s)
          +++ OK, passed 100 tests.
  Pipelined :. SHA-256 @X @Y
    Properties
      (checked by QuickCheck)
        Hashes random single block:                         OK (32.79s)
          +++ OK, passed 100 tests.
      (checked by SmallCheck)
        Hash 'The quick brown fox jumps over the lazy dog': OK (7.79s)
          36 tests completed (but 1 did not meet the condition)
All 4 tests passed (32.80s)
```

