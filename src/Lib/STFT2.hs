{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

{-

A lot of error handling needs to be added to the code. For example
0 < hopsize < windowsize < fftsize needs to be reinforced.
Code currently just throws exceptions. Maybe change to something like;
http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/what_do_you_mean.html

-}

module Lib.STFT2 where

import qualified Data.Complex                  as C
import qualified Data.Finite                   as F
import           Data.Proxy
import           Data.Type.Equality
import qualified Data.Vector.Sized             as V
import           GHC.TypeNats
import           Numeric.FFT.Vector.Invertible

newtype Window n a = Window {
  mkWindow :: V.Vector n a
} deriving (Eq, Ord, Show, Num, Functor, Semigroup, Monoid)

-- A hamming window of size n.
hamming :: forall n p. KnownNat n => p n -> Window n Double
hamming proxy = Window $ V.generate' proxy goHamm where
  size = natVal proxy
  goHamm :: F.Finite n -> Double
  goHamm findex =
    let index = F.getFinite findex
     in 0.54 - 0.46 * cos (2 * pi * fromIntegral index / (fromIntegral size - 1))


hammingComplex :: forall n p. KnownNat n => p n -> Window n (C.Complex Double)
hammingComplex proxy = Window $ V.generate' proxy goHamm where
  size = natVal proxy
  goHamm :: F.Finite n -> C.Complex Double
  goHamm findex =
    let index = F.getFinite findex
     in (C.:+ 0)0.54 - 0.46 * cos (2 * pi * fromIntegral index / (fromIntegral size - 1))

-- A small number
epsilon :: Double
epsilon = 2.2204460492503131e-16

newtype MagnitudeSpectrum n = Mag {
  mkMag :: V.Vector n Double
}

newtype PhaseSpectrum n = Phase {
  mkPhase :: V.Vector n Double
}

newtype FFTsz n = FFTsz {
  mkFFTsz :: F.Finite n
}

newtype Winsz n = Winsz {
  mkWinsz :: F.Finite n
}

newtype Hopsz n = Hopsz {
  mkHopsz :: F.Finite n
}


-- Phase unwrapping algorithm.
-- Converting to and from list for pattern matching is a little cumbersome.
unwrap :: forall n. KnownNat n => PhaseSpectrum n -> Maybe (PhaseSpectrum n)
unwrap (Phase xs) = Phase <$> V.fromListN' (Proxy :: Proxy n) (diff (V.toList xs) 0) where
  diff (x:y:ys) accm
    | (y+accm) - x > pi    = x: diff (y+accm-(2*pi):ys) (accm-(2*pi))
    | (y+accm) - x < (-pi) = x: diff (y+accm+(2*pi):ys) (accm+(2*pi))
    | otherwise             = x: diff (y+accm:ys) accm
  diff [x] _     = [x]
  diff []  _     = []

{-
hM1 :: Int -> Int
hM1 win = floor (fromIntegral (win + 1) / 2 :: Double)

hM2 :: Int -> Int
hM2 win = floor (fromIntegral win / 2 :: Double)

-- Takes a complex signal and fftsz does zero phase windows
zeroPhaseWindow :: CSignal -> FFTsz -> CSignal
zeroPhaseWindow xs fftsz = let win = V.length xs
                               hM1' = hM1 win
                               hM2' = hM2 win
                               zs = V.replicate (fftsz - hM1' - hM2') (c 0)
                            in V.concat [V.slice hM2' hM1' xs, zs,
                                         V.slice 0 hM2' xs]

-- Calculates the magnitude spectrum in Db
magSpect :: CSignal -> MagSpect
magSpect = V.map (dB. near0 . C.magnitude) where
  dB x = 20 * logBase 10 x
  near0 x = if nearZero x then epsilon else x

-- Calculates the phase spectrum
phaseSpect :: CSignal -> PhaseSpect
phaseSpect vec = unwrap $ V.map (C.phase . to0) vec where
  to0 x = go (C.realPart x) (C.imagPart x)
  go x y | nearZero x && nearZero y = 0 C.:+ 0
         | nearZero x               = 0 C.:+ y
         | nearZero y               = x C.:+ 0
         | otherwise                = x C.:+ y


-- Takes a windowed signal and FFT size returns a vector of tuples
-- (Magnitude, Phase). Magnitude in Db, phase unwrapped, both positve
-- half of the spectrum.
dftAnal :: FFTsz -> CSignal -> (MagSpect, PhaseSpect)
dftAnal fftsz winSig = (mag, phase) where
    inputVec = zeroPhaseWindow winSig fftsz
    hN = fftsz `div` 2 + 1
    trsf :: Vector (C.Complex Double)
    trsf = V.slice 0 hN (run dft inputVec)
    mag = magSpect trsf
    phase = phaseSpect trsf


-- Takes an input signal, a window, an fftsize and a hop size and
-- computes the short time Fourier transform, returning a list of
-- (vector of magnitude, vector of phase)
stft :: Signal -> CWindow ->
  FFTsz -> Hopsz -> [(MagSpect, PhaseSpect)]
stft dsig win fftsz hopsz = let sig = V.map c dsig -- Convert to complex
                                winsz = V.length win
                                wdiv = V.sum win
                                w = V.map (/wdiv) win -- Normalize window function
                                halfWin = floor (fromIntegral winsz / 2 :: Double)
                                zs = V.replicate halfWin (c 0)
                                sigzs = V.concat [zs, sig, zs]
                                splitSig = divvy winsz hopsz sigzs
                             in fmap (dftAnal fftsz . V.zipWith (*) w) splitSig


-- Takes a vector of tuples (Magnitude, Phase) and a window size
-- and returns the original signal
dftSynth :: Winsz -> (MagSpect, PhaseSpect) -> Signal
dftSynth win (magVec, phaseVec) =
  let vec = V.zipWith (\x y -> (x, y)) magVec phaseVec
      posFreqs = V.map f vec where
        f (mag, phase) = (10**(mag/20) C.:+ 0) *
                         exp ((phase C.:+ 0) *
                         (0 C.:+ 1))
      negFreqs = (V.map f . V.reverse) (g vec) where
        g = (V.init . V.tail)
        f (mag, phase) = (10**(mag/20) C.:+ 0) *
                         exp ((phase C.:+ 0) *
                         (0 C.:+ (-1)))
      resultidft = run idft (posFreqs V.++ negFreqs)
  in V.map C.realPart $
     V.concat [V.slice (V.length resultidft - hM2 win) (hM2 win) resultidft
              , V.take (hM1 win) resultidft]


stftSynth :: [(MagSpect, PhaseSpect)] -> Hopsz -> Winsz -> Signal
stftSynth magphase hopsz winsz =
  let signalFrames = fmap (V.map (fromIntegral hopsz * ) . dftSynth winsz ) magphase
      signalTuples = fmap (V.splitAt $ hM1 winsz) signalFrames
      overlapAdd (x1, x2) (y1, y2) = (x1 V.++ V.zipWith (+) x2 y1, y2)
   in V.drop (hM1 winsz) $ fst (L.foldl' overlapAdd
                                        (Prelude.head signalTuples) (Prelude.tail signalTuples))
-}
