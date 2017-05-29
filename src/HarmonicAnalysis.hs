{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

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

-- quick conversion to complex
c :: Num a => a -> Comp.Complex a
c = (Comp.:+ 0)

localMaxima :: Ord a => [a] -> [a]
localMaxima (x:rest@(y:z:_))
  | y > x && y > z = y : localMaxima rest
  | otherwise      = localMaxima rest
localMaxima _ = []

filterThreshold :: (Num a, Ord a) => a -> [a] -> [a]
filterThreshold t xs = fmap (\x -> if x >= t then x else 0) xs

type Magnitudes = [Float]
type Threshold = Int

peakDetection :: (Num a , Ord a) => a -> [a] -> [a]
peakDetection t ms = localMaxima $ filterThreshold t ms


calcVertex :: (Eq a, Fractional a) => (a, a) -> (a, a) -> (a, a) -> (a -> a)
calcVertex (x1, y1) (x2, y2) (x3, y3)
  | x1 == x2 || x2 == x1 || x2 == x3 = error "shouldn't error"
  | otherwise = \x -> a*x^2 + b*x + c where
           denom = (x1 - x2)*(x1 - x3)*(x2 - x3)
           a = (x3 * (y2 - y1) + x2 * (y1 - y3) + x1 * (y3 - y2)) / denom
           b = (x3^2 * (y1 - y2) + x2^2 * (y3 - y1) + x1^2 * (y2 - y3)) / denom
           c = (x2 * x3 * (x2 - x3) * y1 +
                x3 * x1 * (x3 - x1) * y2 +
                x1 * x2 * (x1 - x2) * y3) / denom


type SampleRate = Int

-- This is really bad time and space complexity (O(n^3) total) converting to
-- and from list for pattern matching. See suggestions here for improvement;
-- http://stackoverflow.com/questions/36993937/haskell-pattern-matching-on-vectors
unwrap :: Vector Double -> Vector Double
unwrap xs = fromList $ diff (toList xs) 0 where
  diff (x:y:ys) accm
    | (y+accm) - x > pi    = x: diff (y+accm-(2*pi):ys) (accm-(2*pi))
    | (y+accm) - x < (-pi) = x: diff (y+accm+(2*pi):ys) (accm+(2*pi))
    | otherwise             = x: diff (y+accm:ys) accm
  diff [x] _     = [x]
  diff []  _     = []

--Takes a vector and does a zero phase window with an
--FFT size equal to n
zeroPhaseWindow :: Vector (Comp.Complex Double)  -> Int -> Vector (Comp.Complex Double)
zeroPhaseWindow xs fftsz = let win = Vec.length xs
                               hM1 = floor $ fromIntegral (win + 1) / 2
                               hM2 = floor $ (fromIntegral win) / 2
                               zs = Vec.replicate (fftsz-hM1-hM2) (c 0)
                            in Vec.concat [(Vec.slice hM2 hM1 xs), zs,
                                               (Vec.slice 0 hM2 xs)]

-- returns a hamming window of size n
hamming :: Int -> Vector (Comp.Complex Double)
hamming m = generate m hamming' where
  hamming' n = c $ 0.54 - 0.46*cos(2 * pi * (fromIntegral n)/(fromIntegral m-1))

zipNorm :: Vector (Comp.Complex Double) -> Vector (Comp.Complex Double) ->
                                           Vector (Comp.Complex Double)
zipNorm sig win = let wdiv = Vec.sum win
                      w = Vec.map (/wdiv) win
                   in Vec.zipWith (*) w sig

cutToWindow :: Vector (Comp.Complex Double) -> Int -> Double -> Int -> Vector (Comp.Complex Double)
cutToWindow sig winsz start fs = let startSamp = floor $ (fromIntegral fs) * start
                                  in Vec.slice startSamp winsz sig

normTo0Db :: Vector (Double,Double) -> Vector (Double, Double)
normTo0Db xs = Vec.map (first ((-) (fst $ Vec.maximumBy compare xs))) xs

twm :: Vector a -> SampleRate -> Vector a
twm = undefined

-- Next power of 2 greater than n
pO2GTn :: Int -> Int
pO2GTn n = 2^(ceiling $ logBase 2 (fromIntegral n))

magSpectrum :: Vector (Comp.Complex Double) -> Vector Double
magSpectrum vec = Vec.map (dB.(near0).(Comp.magnitude)) vec where
  dB x = 20 * (logBase 10 x)
  near0 x = if nearZero x then epsilon else x

phaseSpectrum :: Vector (Comp.Complex Double) -> Vector Double
phaseSpectrum vec = Vec.map ((Comp.phase).to0) vec where
  to0 x = go (Comp.realPart x) (Comp.imagPart x)
  go x y
    | nearZero x && nearZero y = (0 Comp.:+ 0)
    | nearZero x               = (0 Comp.:+ y)
    | nearZero y               = (x Comp.:+ 0)
    | otherwise                = (x Comp.:+ y)

-- Takes a windowed signal and FFT size returns a vector of tuples
-- (Magnitude, Phase). Magnitude in Db, phase unwrapped, both positve
-- half of the spectrum. FFT size is the next power of 2 greater than
-- the window size.
dftAnal :: Vector (Comp.Complex Double) -> Int -> Vector (Double, Double)
dftAnal winSig fftsz = let inputVec = zeroPhaseWindow winSig fftsz
                           hN = fftsz `div` 2 + 1
                           trsf :: Vector (Comp.Complex Double)
                           trsf = Vec.slice 0 hN (run dft inputVec)
                           mag = magSpectrum trsf
                           phase = phaseSpectrum trsf
                        in Vec.zip mag (unwrap phase)

stft :: Vector (Comp.Complex Double) -> Vector (Comp.Complex Double) ->
                          Int -> Int -> Vector (Double, Double)
stft sig win fftsz hopsz = let m = Vec.length win
                               hM1 = floor (fromIntegral m) / 2 +1
                               hM2 = floor (fromIntegral m) / 2
                               zs = Vec.replicate hM2 (c 0)
                               sigzs = Vec.concat [zs, sig, zs]


-- Takes a vector of tuples (Magnitude, Phase) and a window size
-- and returns the original signal
dftSynth :: Vector (Double, Double) -> Int -> Either String (Vector Double)
dftSynth vec win = let hN = Vec.length vec
                       n = (hN-1)*2
                    in
                       case isPo2 (fromIntegral n) of
                         True -> let hM1 = floor $ fromIntegral (win+1) / 2
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
                                 in Right $ Vec.concat [(Vec.slice (n-hM2) hM2 x), (Vec.slice 0 hM1 x)]
                         _    -> Left $ "n is not a power of 2"

-- linear interpolation
linInterp :: Num a => [a] -> [(a, a)] -> [a]
linInterp = undefined

-- parabolic interpolation
paraInterp :: Num a => [a] -> [(a, a)] -> [a]
paraInterp = undefined

-- Takes a vector of doubles, the sample frequency, and a name
-- and writes the audio file. Written in 32 bit.
outMain :: Vector Double -> Int -> String -> IO ()
outMain vec sf name = let samples = fmap ((:[]) . doubleToSample) (Vec.toList vec)
                          header = WAVEHeader 1 sf 32 Nothing
                       in putWAVEFile name (WAVE header samples)

isPo2 :: (Ord a, Fractional a) => a -> Bool
isPo2 x
  | x > 2     = isPo2 (x / 2)
  | x == 2    = True
  | otherwise = False

wrVec :: Vector (Comp.Complex Double) -> IO ()
wrVec vec = writeFile "prefft.txt" $ Prelude.concat $ Pre.map f (toList vec) where
  f x = "(" Pre.++ (show $ Comp.realPart x) Pre.++ (g $ show $ Comp.imagPart x) Pre.++ "j)" Pre.++ "\n"
  g s = if (Prelude.head s) == '-' then s else "+" Pre.++ s

prVec :: Show a => Int -> Int -> Vector a -> IO ()
prVec x y vec = Vec.mapM_ print (Vec.slice x y vec)

main :: IO ()
main = do
  audio <- getWAVEFile "singing-female.wav"
  let header = waveHeader audio
      samples = waveSamples audio
      channels = waveNumChannels header
      sampRate = waveFrameRate header
  case channels of
    1 -> do putStrLn $ "rate = " Pre.++ show sampRate
            let sig = fromList $ fmap (c.sampleToDouble.(Prelude.head)) samples
                winsz = 511
                fftsz = 1024
                xs = cutToWindow sig winsz 0.2 sampRate
                window = hamming winsz
                wx = zipNorm xs window
                anal = dftAnal wx fftsz
            prVec 0 (Vec.length anal) anal

    _ -> putStrLn "Should be mono."
