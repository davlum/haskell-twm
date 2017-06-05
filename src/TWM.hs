module Main where

import STFT
import Window
import qualified Data.Vector as V
import Data.Bifunctor (first)
import Data.Ord (comparing)
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Fixed (mod')

minf0 = 80 -- minimum possible fundamental
maxf0 = 3000 -- maximum possible fundamental
f0et = 5.0 -- maximum error threshold


localMaxima :: [Double] -> [Double]
localMaxima (x:rest@(y:z:_))
  | y > x && y > z = y : localMaxima rest
  | otherwise      = 0 : localMaxima rest
localMaxima _ = []

filterThreshold :: Int -> V.Vector Double -> V.Vector Double
filterThreshold thresh vec = V.map (\x -> if x > fromIntegral thresh then x else 0) vec

peakDetection :: Int -> V.Vector Double -> V.Vector (Int, Double)
peakDetection thresh vec = V.filter g (V.indexed peaks) where
  g (ind, val) = val /= 0
  filtVec =  filterThreshold thresh vec
  peaks = V.fromList $ 0:(localMaxima $ V.toList filtVec)
  indpeaks = V.indexed peaks

-- takes a vector of magnitudes, a vector of indices of the peak magintudes,
-- and returns a vector of interpolated locations and magnitudes for the peaks
peakInterp :: V.Vector Double -> V.Vector (Int, Double) -> V.Vector (Double, Double)
peakInterp mag peaks = V.map iPM peaks where
    sides ind = (ind-1, ind, ind+1)
    f = ((V.!)mag)
    vals (i1, i2, i3) = (f i1, f i2, f i3)
    iploc p (l, m, r) = (fromIntegral p) + 0.5*(l-r)/(l-2*m+r)
    iPM :: (Int, Double) -> (Double, Double)
    iPM (i, _) = let vs@(l,m,r) = vals $ sides $ i
                     ip = iploc i vs
                  in (ip, (m - 0.25*(l-r)*(ip-(fromIntegral i))))

-- turns a vector of (locations, magnitude) into a vector of (freqs, magnitude).
iFreqs :: Int -> Int -> V.Vector (Double, Double) -> V.Vector (Double, Double)
iFreqs fs fftsz vec = V.map f vec where
  f =  first ((*(fromIntegral fs)).(/(fromIntegral fftsz)))

-- takes Vector of (freq, magnitudes) a vector (freq, mag) of the f0
-- candidates and returns an f0 candidate.
-- Very ugly. Should refactor.
f0Twm :: V.Vector (Double, Double) -> Double -> Double
f0Twm freqMag f0t
    | V.length freqMag < 3 && f0t == 0 = 0
    | V.null f0c = 0
    | otherwise = if f0t <= 0 then twm vec f0c
                  -- If there is no stable previous candidate tehn
                  -- just run twm, else only check close peaks.
                    else twm vec (modls shortList)
   where
    f0c = (V.filter (\(freq, _) -> freq > minf0 && freq < maxf0) freqMag)
    maxcTup = V.maximumBy (comparing snd) f0c
    maxcfd = fst maxcTup `mod'` f0t
    shortList :: V.Vector (Double, Double)
    shortList = V.filter f f0c
    f (freq, mag) = (abs (freq - f0t)) < (f0t/2)
    modmax x = if x > f0t/2 then f0t - x else x
    modls v = if V.elem maxcTup v && (modmax maxcfd) >(f0t/4)
                 then V.cons maxcTup v else v

maxErr :: (Double, Double) -> Double -> Double
maxErr (peak, err) et
  | err < et = peak
  | otherwise = 0

twm :: V.Vector (Double, Double) -> V.Vector (Double, Double) -> (Double, Double)
twm freqMag peaks
    | V.null peaks = (0, 0)
    | otherwise = (f0, err)
  where
    -- Values for the Two way mismatch algorithm
    p = 0.5
    q = 1.4
    r = 0.5
    rho = 0.33
    maxnpeaks = 10
    aMax = (V.maximumBy (comparing snd) magfreq)
    fMax = (V.maxmumBy (comparing fst)



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

main :: IO ()
main = do
  print $ peakDetection 0 (V.fromList [1,2,1,4,5,1])
