# dhall-ats

This is a small example demonstrating how to use Dhall in ATS.

## Building

First, install [cabal](https://www.haskell.org/cabal/download.html) and [GHC
8.2.2](https://www.haskell.org/ghc/download_ghc_8_2_2.html). Then install
[atspkg](http://hackage.haskell.org/package/ats-pkg) with

```
curl -sSl https://raw.githubusercontent.com/vmchale/atspkg/master/bash/install.sh | bash -s
```

Then, run 

```
atspkg test
```

and see the result!
