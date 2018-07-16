**Haskell Two-Way Mismatch**

Implementation of the [two-way mismatch algorithm](https://pdfs.semanticscholar.org/c94b/56f21f32b3b7a9575ced317e3de9b2ad9081.pdf) for fundamental frequency detection. Includes an implementation of the short-time Fourier transform in Haskell. A small UI lets you record a piece of audio and tell you the most common note. Further plans would be to suggest a harmony.

**Requirements**

This currently requires an installation of fftw, although there may be a switch to a [pure Haskell implementation](https://hackage.haskell.org/package/arb-fft) in the future.
For now the front end is just the cloned repository for the [web-dictaphone](https://github.com/mdn/web-dictaphone).
