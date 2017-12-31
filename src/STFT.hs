{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

A lot of error handling needs to be added to the code. For example
0 < hopsize < windowsize < fftsize needs to be reinforced.
Code currently just throws exceptions. Maybe change to something like;
http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/what_do_you_mean.html

-}

module STFT (

    stft
  , stftSynth
  , hammingC
  , Signal
  , CSignal
  , MagSpect
  , PhaseSpect
  , FFTsz
  , Hopsz
  , Winsz

  ) where

import qualified Data.Complex                  as C
import           Data.List                     as L
import           Data.Vector
import qualified Data.Vector.Generic           as V
import           Data.Vector.Split             (divvy)
import           Numeric.FFT.Vector.Invertible
import           Window

epsilon :: Double
epsilon = 2.2204460492503131e-16

-- This code from the Linear.Epsilon library
-- by Edward Kmett. There were some dependency conflicts
-- so I just copied it in. Will fix.
class Num a => Epsilon a where
  -- | Determine if a quantity is near zero.
  nearZero :: a -> Bool

-- | @'abs' a '<=' 1e-6@
instance Epsilon Float where
  nearZero a = abs a <= 1e-6

-- | @'abs' a '<=' 1e-12@
instance Epsilon Double where
  nearZero a = abs a <= 1e-12

type MagSpect = Vector Double -- Magnitude Spectrum

type PhaseSpect = Vector Double -- Phase Spectrum

type CSignal = Vector (C.Complex Double) -- Complex Signal

type Signal = Vector Double

type FFTsz = Int -- Should be Greater than 0 and a power of 2

type Hopsz = Int -- Should be about 1/4 the fft sz

type Winsz = Int -- Should be an odd number and smaller than FFTsz


-- Phase unwrapping algorithm.
-- Converting to and from list for pattern matching is a little cumbersome.
unwrap :: PhaseSpect -> PhaseSpect
unwrap xs = fromList $ diff (toList xs) 0 where
  diff (x:y:ys) accm
    | (y+accm) - x > pi    = x: diff (y+accm-(2*pi):ys) (accm-(2*pi))
    | (y+accm) - x < (-pi) = x: diff (y+accm+(2*pi):ys) (accm+(2*pi))
    | otherwise             = x: diff (y+accm:ys) accm
  diff [x] _     = [x]
  diff []  _     = []


hM1 :: Int -> Int
hM1 win = floor $ (fromIntegral (win + 1) / 2 :: Double)

hM2 :: Int -> Int
hM2 win = floor $ (fromIntegral win / 2 :: Double)

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
                                halfWin = floor $ (fromIntegral winsz / 2 :: Double)
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
stftSynth magphase hopsz winsz = let signalFrames = fmap (V.map (fromIntegral hopsz * ) . dftSynth winsz ) magphase
                                     signalTuples = fmap (V.splitAt $ hM1 winsz) signalFrames
                                     overlapAdd (x1, x2) (y1, y2) = (x1 V.++ V.zipWith (+) x2 y1, y2)
                                  in V.drop (hM1 winsz) $ fst (L.foldl' overlapAdd
                                        (Prelude.head signalTuples) (Prelude.tail signalTuples))
