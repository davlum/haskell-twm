{-# LANGUAGE GADTs #-}

module Lib.BeatDetection where

import qualified Data.List        as Ls
import qualified Data.List.Split  as LsSplit
import qualified Data.Vector      as Vec
import qualified Lib.TWM          as TWM
import qualified System.Directory as Sys

data Numerator = Three | Four

class IntWrapper a where
  eval :: a -> Int

newtype SamplingRate = SamplingRate Int

newtype ImpulseSize = ImpulseSize Int

newtype BPM = BPM Int

instance IntWrapper BPM where
  eval (BPM x) = x

instance IntWrapper SamplingRate where
  eval (SamplingRate x) = x

instance IntWrapper ImpulseSize where
  eval (ImpulseSize x) = x


sumSquare :: Num a => a -> a -> a
sumSquare x y = (x * y) ^ (2 :: Int)

energy :: Num a => [a] -> a
energy = Ls.foldl' (\x y -> x + (y ^ (2 :: Int))) 0

energyMap :: Num a => ImpulseSize -> [a] -> [a]
energyMap impulseSize ls = fmap energy (LsSplit.chunksOf (eval impulseSize) ls)

-- List of energy per implulse
energyPerImpulse :: Num a => ImpulseSize -> SamplingRate -> [a] -> [a]
energyPerImpulse impulseSize samplingRate sig =
  concat $ fmap (energyMap impulseSize) (LsSplit.chunksOf (eval samplingRate) sig)

-- infinite list of the metronome signal
mkMetronomeSignal :: Numerator -> ImpulseSize -> SamplingRate -> BPM -> [Double]
mkMetronomeSignal timeSig imp samp bpm =
  let impulsesPerSecond :: Double
      impulsesPerSecond = fromIntegral (eval samp) / fromIntegral (eval imp)
      impulsesPerBeat = impulsesPerSecond * (fromIntegral (eval bpm) / 60)
      zsByN n = replicate (floor $ (impulsesPerBeat / n) - 1)  0
      zsBy4 = zsByN 8
      zsBy3 = zsByN 6
   in case timeSig of
        Three -> concat $ repeat $ 1 : zsBy3 ++ 0.5 : zsBy3 ++ 0.5 : zsBy3
        Four  -> concat $ repeat $ 1 : zsBy4 ++ 0.5 : zsBy4 ++ 0.75 : zsBy4 ++ 0.5 : zsBy4

correlate :: (Num a, Ord a) => [a] -> [a] -> [a]
correlate xs ys =
  case compare (length xs) (length ys) of
    LT -> correlate ys xs
    _ -> fmap (sum . zipWith sumSquare ys) divvySig where
      smallSigLen = length ys
      divvySig = LsSplit.divvy smallSigLen 1 xs

runningSum :: Num a => [a] -> [a]
runningSum = go 0 where
  go _ [] = []
  go n [x] = [n + x]
  go n (x:xs) =
    let s = n + x
     in s : go s xs

difference :: Num a => [a] -> [a]
difference [] = []
difference [i] = [i]
difference (i:j:is) = i : go j ((j - i):is)
  where
    go :: Num a => a -> [a] -> [a]
    go _ []       = []
    go _ [x]      = [x]
    go n [x, y]   = x : go y [y - n]
    go n (x:y:xs) = x : go y ((y - n):xs)

{-
energyPerSecond :: Int -> Int -> V.Vector Double -> [Bool]
energyPerSecond blocksz fs sig =
  let k = 1 / (fromIntegral fs / fromIntegral blocksz) :: Double
      splitSig = splitEvery fs sig
      energyLs = V.map (energyMap blocksz) splitSig
      avgLs = V.map ((*k) . V.sum) energyLs
      variances = V.zipWith variance avgLs energyLs
      constLs = V.map regress variances
      constAvgLs = V.zipWith (*) avgLs constLs

      regress :: Double -> Double
      regress x = (x * negate 0.0000015) + 1.5142857

      variance :: Double -> V.Vector Double -> Double
      variance avg energies = k * V.foldl1' var energies
        where
          var x y = x + ((avg - y) ^ (2 :: Int))

      vecBools = V.zipWith (\c enLs -> V.map (> c) enLs) constAvgLs energyLs
  in concat $ V.toList $ fmap V.toList vecBools
-}

runDetect :: IO ()
runDetect = do
  path <- Sys.getCurrentDirectory
  let imp = ImpulseSize 1000
      fullPath = path ++ "/assets/singing-female.wav"
  (fs, sig) <- TWM.readWav fullPath
  let sigLs = Vec.toList sig
      minVal = abs $ minimum sigLs
      posSig = fmap (+ minVal) sigLs
  print posSig

