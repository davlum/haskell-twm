{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

--import Numeric.FFT
import qualified GHC.Base as Pre
import Data.WAVE
import qualified Data.Vector.Generic as Vec
import Numeric.FFT.Vector.Invertible
import Data.Vector
import qualified Data.Complex as Comp
import Data.Bifunctor (first)
import Linear.Epsilon

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


-- Seems to produce OK results, but algorithm is technically
-- incorrect. 2*pi should be applied to the entire tail
-- of the vector.
unwrap :: Vector Double -> Vector Double
unwrap xs = scanl' diff (Vec.head xs) (Vec.tail xs) where
  diff x y
    | y - x > pi    = y - 2*pi
    | y - x < (-pi) = y + 2*pi
    | otherwise     = y

unwrap' :: Vector Double -> Vector Double
unwrap' vec = fromList (go (toList vec)) where
  go (x:rest@(y:_))
    | y - x > pi    = x: go (fmap (-(2*pi)) rest)
    | y - x < (-pi) = x: go (fmap (+(2*pi)) rest)
    | otherwise     = x: go rest
    go _            = []


-- Takes a vector of complex values, converts it to magnitude and phase.
-- The magnitude is then coverted to Db and the phase is unwrapped.
calcSpecs :: Vector (Comp.Complex Double) -> Vector (Double, Double)
calcSpecs xs = scanl' diff (dB $ Comp.polar (Vec.head xs)) (Vec.tail xs) where
  dB x
    | nearZero $ fst x = x
    | otherwise        = first (\y -> 20 * log (abs y)) x
  diff x y = go x (dB $ Comp.polar y) where
    go (_, ph1) (mag2, ph2)
      | ph2 - ph1 > pi    = (mag2, ph2 - 2*pi)
      | ph2 - ph1 < (-pi) = (mag2, ph2 + 2*pi)
      | otherwise         = (mag2, ph2)

--Takes a vector and does a zero phase window with an
--FFT size equal to n
zeroPhaseWindow :: RealFloat a => Vector a -> Int -> Vector a
zeroPhaseWindow xs win = let hM1 = floor $ (fromIntegral (win + 1)) / 2
                             hM2 = floor $ (fromIntegral win) / 2
                             zs = Vec.replicate ((pO2GTn win)-hM1-hM2) 0.0
                       in Vec.concat [(Vec.slice hM2 hM1 xs), zs,
                                      (Vec.slice 0 hM2 xs)]

-- returns a hamming window of size n
hamming :: RealFloat a => Int -> Vector a
hamming m = generate m hamming' where
  hamming' n = 0.54 - 0.46*cos(2 * pi * (fromIntegral n)/(fromIntegral m-1))

normTo0Db :: Vector (Double,Double) -> Vector (Double, Double)
normTo0Db xs = Vec.map (first ((-) (fst $ Vec.maximumBy compare xs))) xs

twm :: Vector a -> SampleRate -> Vector a
twm xs fs = undefined

-- Next power of 2 greater than n
pO2GTn :: Int -> Int
pO2GTn n = 2^(ceiling $ logBase 2 (fromIntegral n))

-- Takes an input vector and window size returns a vector of tuples
-- (Magnitude, Phase). Magnitude in Db, phase unwrapped, both positve
-- half of the spectrum. FFT size is the next power of 2 greater than
-- the window size.
dftAnal :: Vector Double -> Int -> Vector (Double, Double)
dftAnal xs win = let wx = Vec.zipWith (*) (hamming win) xs
                     inputVec = zeroPhaseWindow wx win
                     hN = (Vec.length inputVec) `div` 2 + 1
                     x :: Vector (Comp.Complex Double)
                     x = run dftR2C inputVec
                 in calcSpecs (Vec.slice 0 hN x)


-- Takes a vector of tuples (Magnitude, Phase) and a window size
-- and writes the file.
dftSynth :: Vector (Double, Double) -> Int -> Vector Double
dftSynth vec win = let hN = Vec.length vec
                       n = (hN-1)*2
                       hM1 = floor $ fromIntegral (win+1) / 2
                       hM2 = floor $ fromIntegral win / 2
                       zs :: Vector (Comp.Complex Double)
                       zs = Vec.replicate (n - hM1 - hM2) 0.0
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
                                                                zs, negFreqs]))
                    in Vec.concat [(Vec.slice hM2 hM1 x), (Vec.slice 0 hM2 x)]


-- Gives magnitude spectrum until n in Db
magSpectrum :: (Epsilon a, RealFloat a) => Vector a -> Int -> Vector a
magSpectrum xs n = Vec.map f (Vec.slice 0 n xs) where
  f x
    | nearZero x = x
    | otherwise  = 20 * log (abs x)

-- linear interpolation
linInterp :: Num a => [a] -> [(a, a)] -> [a]
linInterp = undefined

--
paraInterp :: Num a => [a] -> [(a, a)] -> [a]
paraInterp = undefined

-- Takes a vector of doubles, the sample frequency, and a name
-- and writes the audio file. Written in 32 bit.
outMain :: Vector Double -> Int -> String -> IO ()
outMain vec sf name = let samples = fmap ((:[]) . doubleToSample) (Vec.toList vec)
                          header = WAVEHeader 1 sf 32 Nothing
                       in putWAVEFile name (WAVE header samples)

main :: IO ()
main = do
  path <- getLine
  audio <- getWAVEFile path
  let header = waveHeader audio
      samples = waveSamples audio
      channels = waveNumChannels header
      sampRate = waveFrameRate header
  case channels of
    1 -> do putStrLn $ "rate = " Pre.++ show sampRate
            let inputVec = fromList $ fmap (sampleToDouble.(Prelude.head)) samples
                win = let f = \x -> if even x then x - 1 else x
                       in f $ Vec.length inputVec
                analysis = dftAnal inputVec win
            print analysis
            let trsf = dftSynth (analysis) win
            outMain trsf sampRate "test.wav"
    _ -> putStrLn "Should be mono."


{-
main :: IO ()
main = do
  path <- getLine
  audio <- getWAVEFile path
  let header = waveHeader audio
      samples = waveSamples audio
      channels = waveNumChannels header
      sampRate = waveFrameRate header
  case channels of
    1 -> do putStrLn $ "rate = " Pre.++ show sampRate
            let inputVec = fromList $ fmap (c.sampleToDouble.(Prelude.head)) samples
                l = Vec.length inputVec
            pl <- Numeric.FFT.plan l
            let trsf = fftWith pl inputVec
                itrsf = Vec.map Comp.realPart (ifftWith pl trsf)
            outMain itrsf sampRate "test.wav"
    _ -> putStrLn "Should be mono."
-}
