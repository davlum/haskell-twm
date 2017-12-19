{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

A lot of error handling needs to be added to the code. For example
0 < hopsize < windowsize < fftsize needs to be reinforced.
Code currently just throws exceptions. Maybe change to something like;
http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/what_do_you_mean.html

-}

module STFT (

    stft
  , readWav
  , writeWav
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

import System.IO (writeFile)
import Data.WAVE
import Window
import qualified Data.Vector.Generic as V
import Numeric.FFT.Vector.Invertible
import Data.Vector
import qualified Data.Complex as C
import Data.Bifunctor (first)
import Data.Vector.Split (divvy)

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

type Path = String

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


-- Takes a complex signal and fftsz does zero phase windows
zeroPhaseWindow :: CSignal -> FFTsz -> CSignal
zeroPhaseWindow xs fftsz = let win = V.length xs
                               hM1 = floor $ fromIntegral (win + 1) / 2
                               hM2 = floor $ fromIntegral win / 2
                               zs = V.replicate (fftsz-hM1-hM2) (c 0)
                            in V.concat [V.slice hM2 hM1 xs, zs,
                                         V.slice 0 hM2 xs]


-- Normalize signal to 0 Db.
normTo0Db :: Vector (Double,Double) -> Vector (Double, Double)
normTo0Db xs = V.map (first ((fst $ V.maximumBy compare xs) -)) xs

-- Next power of 2 greater than n.
po2gtn :: Int -> Int
po2gtn n = 2 ^ ceiling ( logBase 2 (fromIntegral n))

isPo2 :: (Ord a, Fractional a) => a -> Bool
isPo2 x
  | x > 2     = isPo2 (x / 2)
  | x == 2    = True
  | otherwise = False

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
                                halfWin = floor $ fromIntegral winsz / 2
                                zs = V.replicate halfWin (c 0)
                                sigzs = V.concat [zs, sig, zs]
                                splitSig = divvy winsz hopsz sigzs
                             in fmap (dftAnal fftsz . V.zipWith (*) w) splitSig


-- Takes a vector of tuples (Magnitude, Phase) and a window size
-- and returns the original signal
dftSynth :: Winsz -> (MagSpect, PhaseSpect) -> Signal
dftSynth win (magVec, phaseVec) =
  let vec = V.zipWith (\x y -> (x, y)) magVec phaseVec
      hN = V.length vec
      n = (hN-1)*2
      hM1 = floor $ fromIntegral (win+1) / 2
      hM2 = floor $ fromIntegral win / 2
      posFreqs = V.map f vec where
        f (mag, phase) = (10**(mag/20) C.:+ 0) *
                         exp ((phase C.:+ 0) *
                         (0 C.:+ 1))
      negFreqs = (V.map f . V.reverse) (g vec) where
        g = (V.init . V.tail)
        f (mag, phase) = (10**(mag/20) C.:+ 0) *
                         exp ((phase C.:+ 0) *
                         (0 C.:+ (-1)))
      x = run idft (posFreqs V.++ negFreqs)
  in V.map C.realPart $ V.concat [V.slice (V.length x - hM2) hM2 x, V.take hM1 x]


stftSynth :: [(MagSpect, PhaseSpect)] -> Hopsz -> Winsz -> Signal
stftSynth magphase hopsz winsz = let hM1 = floor $ fromIntegral (winsz+1) / 2
                                     hM2 = floor $ fromIntegral winsz / 2
                                     signalFrames = fmap (V.map (fromIntegral hopsz * ) . dftSynth winsz ) magphase
                                     signalTuples = fmap (V.splitAt hM1) signalFrames
                                     overlapAdd (x1, x2) (y1, y2) = (x1 V.++ V.zipWith (+) x2 y1, y2)
                                  in V.drop hM1 $ fst (Prelude.foldl1 overlapAdd
                                        (Prelude.head signalTuples) (Prelude.tail signalTuples))

-- Takes a vector of doubles, the sample frequency, and a name
-- and writes the audio file. Written in 32 bit.
writeWav :: Signal -> Int -> String -> IO ()
writeWav vec sf name = let samples = fmap ((:[]) . doubleToSample) (V.toList vec)
                           header = WAVEHeader 1 sf 32 Nothing
                       in putWAVEFile name (WAVE header samples)

-- Takes a Path and returns IO (sampling frequency, Vector signal).
readWav :: Path -> IO (Int, Signal)
readWav path = do
  audio <- getWAVEFile "singing-female.wav"
  let header = waveHeader audio
      samples = waveSamples audio
      channels = waveNumChannels header
      sampRate = waveFrameRate header
  case channels of
    1 -> let sig = fromList $ fmap (sampleToDouble . Prelude.head) samples
          in return (sampRate, sig)
    _ -> error "Should be mono."
