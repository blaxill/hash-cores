{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc844"
, pkgs ? nixpkgs.pkgs }:
let clash-compiler = pkgs.fetchFromGitHub {
      owner  = "clash-lang";
      repo = "clash-compiler";
      rev  = "67ec42f3df72389b5fc03197b6518d8c96498cc3";
      sha256 = "1j56zx6ry5n8ss4xn9kpfxjdq66c4cxmc78pv2z61cjaxmkv7614";
      fetchSubmodules = true;
    };

    haskellPackages = with pkgs.haskell.lib; pkgs.haskell.packages.ghc844.override {
      overrides = self: super: {
        clash-prelude = dontHaddock (doJailbreak (dontCheck (self.callCabal2nix "clash-prelude" (clash-compiler + "/clash-prelude") {})));
        clash-ghc = enableSharedExecutables (doJailbreak (dontCheck (self.callCabal2nix "clash-ghc" (clash-compiler + "/clash-ghc") {})));
        clash-lib = dontCheck (self.callCabal2nix "clash-lib" (clash-compiler + "/clash-lib") {});

        ghc-tcplugins-extra = self.callCabal2nix "ghc-tcplugins-extra" (pkgs.fetchFromGitHub{
          owner  = "clash-lang";
          repo = "ghc-tcplugins-extra";
          rev  = "ac70960df5b04ec092ea189c8d34b28ab9b41695";
          sha256 = "0ghfndlzw3rdnyxyxjgdbmcnkk985x65wga00ky1acxhlq6md4n4";
        }) {};
        ghc-typelits-extra = self.callCabal2nix "ghc-typelits-extra" (pkgs.fetchFromGitHub{
          owner  = "clash-lang";
          repo = "ghc-typelits-extra";
          rev  = "f1cba7cebf73e429dbdfa67c88161300bc5e318e";
          sha256 = "159z5k68yiri75zxp0fxb82clna7c57wll2fwwm17vfhba3780hh";
        }) {};
        ghc-typelits-knownnat = self.callCabal2nix "ghc-typelits-knownnat" (pkgs.fetchFromGitHub{
          owner  = "clash-lang";
          repo = "ghc-typelits-knownnat";
          rev  = "7c866bdefff3f8353a29eebb3d35264dacb2af28";
          sha256 = "1s7xf60f9r2i9xhg9p4prm2qw4rvqag0wx1jsrfzrrx8nm3b53rl";
        }) {};
        ghc-typelits-natnormalise = self.callCabal2nix "ghc-typelits-natnormalise" (pkgs.fetchFromGitHub{
          owner  = "clash-lang";
          repo = "ghc-typelits-natnormalise";
          rev  = "b4951d4d9b7307154eac0984530bf2d70bca3358";
          sha256 = "06y04gxs21h4pd0bl61flfc4jhzfkkhly5vcm6jkn7pcfxnwflk6";
        }) {};
        th-orphans = self.callCabal2nix "th-orphans" (pkgs.fetchFromGitHub{
          owner  = "mgsloan";
          repo = "th-orphans";
          rev  = "10c682ed4488cd10e758793f35d597e7aeccf367";
          sha256 = "15a0x7sii3b4803263napi1s45h6gzzda02msf0igi7xq2245yj7";
        }) {};

        # Used for ghc 8.6.2
        # th-desugar = self.th-desugar_1_9;
        # singletons = self.singletons_2_5;

        hint = dontCheck (doJailbreak super.hint);

        "prettyprinter" = self.callPackage
          ({ mkDerivation, ansi-wl-pprint, base, bytestring, containers
           , criterion, deepseq, doctest, mtl, pgp-wordlist, QuickCheck
           , random, tasty, tasty-hunit, tasty-quickcheck, text, transformers
           }:
           mkDerivation {
             pname = "prettyprinter";
             version = "1.2.1";
             sha256 = "1kvza7jp5n833m8rj0bc35bd2p8wx3fq0iqflm9nbh3wm05kwrg7";
             isLibrary = true;
             isExecutable = true;
             libraryHaskellDepends = [ base text ];
             testHaskellDepends = [
               base bytestring doctest pgp-wordlist tasty tasty-hunit
               tasty-quickcheck text
             ];
             benchmarkHaskellDepends = [
               ansi-wl-pprint base containers criterion deepseq mtl QuickCheck
               random text transformers
             ];
             description = "A modern, easy to use, well-documented, extensible pretty-printer";
             license = pkgs.stdenv.lib.licenses.bsd2;
           }) {};
      };
    };

  # haskellBuildInputs = hp: with hp;
  #   [ clash-ghc
  #   ];
  # ghcEnv = haskellPackages.ghcWithPackages haskellBuildInputs;
  # ghcCommand = "ghc";
  # ghcCommandCaps = pkgs.lib.toUpper ghcCommand;
  # ghc = haskellPackages.ghc;

in
  haskellPackages.callPackage ./hash-cores.nix { }

