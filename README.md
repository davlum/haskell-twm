**Haskell Two-Way Mismatch**

Implementation of the two-way mismatch algorithm for fundamental frequency detection. Includes an implementation of the short-time Fourier transform in Haskell.

**Requirements**

This currently requires an installation of fftw, although there may be a switch to a [pure Haskell implementation](https://hackage.haskell.org/package/arb-fft) in the future.

On MacOS

1. `brew install fftw`
2. `cabal sandbox init`
3. `cabal install --dependencies-only && cabal configure && cabal build`
4. `cabal run`
