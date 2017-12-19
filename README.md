**Haskell Two-Way Mismatch**

Implementation of the two-way mismatch algorithm for fundamental frequency detection. Includes an implementation of the short-time Fourier transform in Haskell.

**Requirements**

This currently requires an installation of fftw, although there may be a switch to a [pure Haskell implementation](https://hackage.haskell.org/package/arb-fft) in the future.
For now the front end is just the cloned repository for the [web-dictaphone](https://github.com/mdn/web-dictaphone).

On MacOS

1. `brew install fftw`
2. `git clone https://github.com/mdn/web-dictaphone.git assets`
3. `cabal sandbox init`
4. `cabal install --dependencies-only && cabal configure && cabal build`
5. `cabal run`
