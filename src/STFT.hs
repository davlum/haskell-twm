{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module STFT (

    stft
  , dftAnal
  , dftSynth
  , readWav
  , writeWav
  , normTo0Db

  ) where

import System.IO (writeFile)
import qualified GHC.Base as Pre
import Data.WAVE
import qualified Data.Vector.Generic as Vec
import Numeric.FFT.Vector.Invertible
import Data.Vector
import qualified Data.Complex as Comp
import Data.Bifunctor (first)
import Linear.Epsilon
import Numeric.IEEE (epsilon)
import Data.Vector.Split (divvy)

-- quick conversion to complex
c :: Num a => a -> Comp.Complex a
c = (Comp.:+ 0)

-- Rewrite to replace divvy function. No use adding another
-- dependency for one function.
{-
splitInto :: Int -> Int -> Vector a -> [Vector a]
splitInto hopsz winsz = unfoldr go where
  go v | Vec.length v < winsz = Nothing
       | otherwise            = Just (Vec.take winsz v, Vec.drop hopsz v)
-}

-- Phase unwrapping algorithm.
-- Converting to and from list for pattern matching is a little cumbersome.
-- See this post for possible improvement;
-- http://stackoverflow.com/questions/36993937/haskell-pattern-matching-on-vectors
unwrap :: Vector Double -> Vector Double
unwrap xs = fromList $ diff (toList xs) 0 where
  diff (x:y:ys) accm
    | (y+accm) - x > pi    = x: diff (y+accm-(2*pi):ys) (accm-(2*pi))
    | (y+accm) - x < (-pi) = x: diff (y+accm+(2*pi):ys) (accm+(2*pi))
    | otherwise             = x: diff (y+accm:ys) accm
  diff [x] _     = [x]
  diff []  _     = []

-- Takes a vector and fftsz does zero phase windows with an
-- FFT size equal to fftsz
zeroPhaseWindow :: Vector (Comp.Complex Double)  -> Int -> Vector (Comp.Complex Double)
zeroPhaseWindow xs fftsz = let win = Vec.length xs
                               hM1 = floor $ fromIntegral (win + 1) / 2
                               hM2 = floor $ (fromIntegral win) / 2
                               zs = Vec.replicate (fftsz-hM1-hM2) (c 0)
                            in Vec.concat [(Vec.slice hM2 hM1 xs), zs,
                                               (Vec.slice 0 hM2 xs)]

-- Normalize signal to 0 Db.
normTo0Db :: Vector (Double,Double) -> Vector (Double, Double)
normTo0Db xs = Vec.map (first ((-) (fst $ Vec.maximumBy compare xs))) xs

-- Next power of 2 greater than n.
pO2GTn :: Int -> Int
pO2GTn n = 2^(ceiling $ logBase 2 (fromIntegral n))

isPo2 :: (Ord a, Fractional a) => a -> Bool
isPo2 x
  | x > 2     = isPo2 (x / 2)
  | x == 2    = True
  | otherwise = False

-- Calculates the magnitude spectrum in Db
magSpectrum :: Vector (Comp.Complex Double) -> Vector Double
magSpectrum vec = Vec.map (dB.(near0).(Comp.magnitude)) vec where
  dB x = 20 * (logBase 10 x)
  near0 x = if nearZero x then epsilon else x

-- Calculates the phase spectrum
phaseSpectrum :: Vector (Comp.Complex Double) -> Vector Double
phaseSpectrum vec = unwrap $ Vec.map ((Comp.phase).to0) vec where
  to0 x = go (Comp.realPart x) (Comp.imagPart x)
  go x y | nearZero x && nearZero y = (0 Comp.:+ 0)
         | nearZero x               = (0 Comp.:+ y)
         | nearZero y               = (x Comp.:+ 0)
         | otherwise                = (x Comp.:+ y)

-- Takes a windowed signal and FFT size returns a vector of tuples
-- (Magnitude, Phase). Magnitude in Db, phase unwrapped, both positve
-- half of the spectrum.
dftAnal :: Int -> Vector (Comp.Complex Double) -> (Vector Double, Vector Double)
dftAnal fftsz winSig = (mag, phase) where
    inputVec = zeroPhaseWindow winSig fftsz
    hN = fftsz `div` 2 + 1
    trsf :: Vector (Comp.Complex Double)
    trsf = Vec.slice 0 hN (run dft inputVec)
    mag = magSpectrum trsf
    phase = phaseSpectrum trsf

-- Takes an input signal, a window, an fftsize and a hop size and
-- computes the short time Fourier transform
stft :: Vector (Comp.Complex Double) -> Vector (Comp.Complex Double) ->
  Int -> Int -> [(Vector Double, Vector Double)]
stft sig win fftsz hopsz = let winsz = Vec.length win
                               wdiv = Vec.sum win
                               w = Vec.map (/wdiv) win
                               halfWin = floor $ (fromIntegral winsz) / 2
                               zs = Vec.replicate halfWin (c 0)
                               sigzs = Vec.concat [zs, sig, zs]
                               splitSig = divvy winsz hopsz sigzs
                            in fmap ((dftAnal fftsz).(Vec.zipWith (*) w)) splitSig

-- Takes a vector of tuples (Magnitude, Phase) and a window size
-- and returns the original signal
dftSynth :: Vector (Double, Double) -> Int -> Vector Double
dftSynth vec win = let hN = Vec.length vec
                       n = (hN-1)*2
                       hM1 = floor $ fromIntegral (win+1) / 2
                       hM2 = floor $ fromIntegral win / 2
                       posFreqs = Vec.map f vec where
                         f (mag, phase) = (10**(mag/20) Comp.:+ 0) *
                                          (exp ((phase Comp.:+ 0) *
                                          (0 Comp.:+ 1)))
                       negFreqs = ((Vec.map f) . Vec.reverse) (g vec) where
                         g = (Vec.init . Vec.tail)
                         f (mag, phase) = (10**(mag/20) Comp.:+ 0) *
                                          (exp ((phase Comp.:+ 0) *
                                          (0 Comp.:+ (-1))))
                       x = Vec.map Comp.realPart (run idft (Vec.concat [posFreqs,
                                                                        negFreqs]))
                   in Vec.concat [(Vec.slice (n-hM2) hM2 x), (Vec.slice 0 hM1 x)]

-- Takes a vector of doubles, the sample frequency, and a name
-- and writes the audio file. Written in 32 bit.
writeWav :: Vector Double -> Int -> String -> IO ()
writeWav vec sf name = let samples = fmap ((:[]) . doubleToSample) (Vec.toList vec)
                           header = WAVEHeader 1 sf 32 Nothing
                       in putWAVEFile name (WAVE header samples)

-- Takes a Path and returns either string error or Vector (sampling frequency, signal).
readWav :: String -> IO (Either String (Int, (Vector (Comp.Complex Double))))
readWav path = do
  audio <- getWAVEFile "singing-female.wav"
  let header = waveHeader audio
      samples = waveSamples audio
      channels = waveNumChannels header
      sampRate = waveFrameRate header
  case channels of
    1 -> let sig = fromList $ fmap (c.sampleToDouble.(Prelude.head)) samples
          in return $ Right $ (sampRate, sig)
    _ -> return $ Left $ "Should be mono."


wrVec :: Vector (Comp.Complex Double) -> IO ()
wrVec vec = writeFile "prefft.txt" $ Prelude.concat $ Pre.map f (toList vec) where
  f x = "(" Pre.++ (show $ Comp.realPart x) Pre.++ (g $ show $ Comp.imagPart x) Pre.++ "j)" Pre.++ "\n"
  g s = if (Prelude.head s) == '-' then s else "+" Pre.++ s

wrVec' :: Vector Double -> IO ()
wrVec' vec = writeFile "prefft.txt" $ Prelude.concat $ fmap show (toList vec) where

prVec :: Show a => Int -> Int -> Vector a -> IO ()
prVec x y vec = Vec.mapM_ print (Vec.slice x y vec)

{-
main :: IO ()
main = do
  maybeAudio <- readWav "singing-female.wav"
  case maybeAudio of
    Left x -> do
             print x
    Right (sf, sig) -> do
      let winsz = 511
          fftsz = 1024
          hopsz = 256
          window = hamming winsz
          anal = stft sig window fftsz hopsz
      writeFile "prefft0.txt" (show $ Prelude.concat $ fmap (toList.fst) anal)
-}
