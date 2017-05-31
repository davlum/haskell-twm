module Main where

import STFT
import Window
import qualified Data.Vector as V

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

iFreqs :: Int -> Int -> Vector (Double, Double)
iFreqs fs fftsz = V.map f where
  f =  ((*fs).(/fftsz).fst)

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
