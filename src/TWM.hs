{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Applicative
import           Data.Bifunctor      (first)
import qualified Data.Complex        as C
import           Data.Fixed          (mod')
import           Data.Foldable
import           Data.List           (scanl')
import           Data.Maybe
import           Data.Ord            (comparing)
import qualified Data.Tree
import qualified Data.Tree           as T
import qualified Data.Vector         as V
import           GHC.Generics
import           STFT
import           Window

data Decision a b = Requirement (a -> Bool) | Action (a -> b)

iff = T.Node . Requirement
action = flip T.Node [] . Action

type Threshold = Int

type Indices = V.Vector Int

type Freq = Double


localMaxima :: [Double] -> [Double]
localMaxima (x:rest@(y:z:_))
  | y > x && y > z = y : localMaxima rest
  | otherwise      = 0 : localMaxima rest
localMaxima _ = []


-- linear interpolation between two points
linInterp :: (Int, Double) -> (Int, Double) -> Double -> Double
linInterp (x0, y0) (x1, y1) x = y0 + (x - fromIntegral x0)*((y1 - y0)/ fromIntegral (x1 - x0))

-- Takes a (sampling rate, Signal), a window function, an FFTsz, A Hopsz,
-- A Threshold, minimum frequency, maximum frequency, maximum error threshold
-- and returns a list of f0 candidates
f0Detection :: (Int, Signal)
            -> CWindow
            -> FFTsz
            -> Hopsz
            -> Threshold
            -> Freq
            -> Freq
            -> Double
            -> [(V.Vector Double, MagSpect, PhaseSpect)]
f0Detection (fs, sig) window fftsz hopsz thresh minfreq maxfreq errmax =
  tail $ scanl scanf0Twm Nothing lsVec where
    sigstft = stft sig window fftsz hopsz
    lsVec = fmap peakMod sigstft
    ipFreqs (a, b, c) = (V.map ((fromIntegral fs *).(/ fromIntegral fftsz)) a, b, c)
    scanf0Twm :: Maybe Freq -> (V.Vector Double, MagSpect, PhaseSpect) -> Maybe Freq
    scanf0Twm f0t (ipfreq, ipmag, _) = f0Twm ipfreq ipmag errmax minfreq maxfreq f0t
    peakMod (magVec, phaseVec) = ipFreqs $ peakInterp magVec phaseVec (peakDetection thresh magVec)


-- Takes a minimum threshold and a vector of magnitudes, and returns
-- a vector of indexes
peakDetection :: Threshold -> MagSpect -> Indices
peakDetection thresh vec = V.map (+1) (V.findIndices (/=0) ploc) where
  indpeaks = V.tail $ V.init $ V.indexed vec
  threshVec = V.map (filtTo0 (fromIntegral thresh) . snd) indpeaks
  next = V.map fmajor indpeaks
  prev = V.map fminor indpeaks
  fmajor (ind, val) = filtTo0 (vec V.! (ind+1)) val
  fminor (ind, val) = filtTo0 (vec V.! (ind-1)) val
  filtTo0 :: Double -> Double -> Double
  filtTo0 t x = if x > t then x else 0
  ploc = V.zipWith3 mult threshVec next prev
  mult x y z = x * y * z


-- takes the magntiude Spectrum, Phase Spectrum, and a vector of indices of the peak magintudes,
-- and returns a vector of interpolated locations and magnitudes for the peaks
peakInterp :: MagSpect -> PhaseSpect -> Indices -> (V.Vector Double, MagSpect, PhaseSpect)
peakInterp mag phase peaks = V.unzip3 (V.map iPM peaks) where
    sides ind = (ind-1, ind, ind+1)
    yVals vec (i1, i2, i3) = (vec V.! i1, vec V.! i2, vec V.! i3)
    iploc p (l, m, r) = fromIntegral p + 0.5 * (l - r)/(l - 2 * m + r)
    iPM i = let adj@(l,m,r) = sides i
                mags@(ml,mm,mr) = yVals mag adj
                iPeak = iploc i mags :: Double
                iMag = mm - 0.25*(ml - mr)*(iPeak- fromIntegral i)
                ind :: Int
                ind = floor iPeak
                -- Phase is decently incorrect.
                iPhase = linInterp (ind, phase V.! ind)
                                   (ind + 1, phase V.! ind + 1) iPeak
             in (iPeak, iMag, iPhase)


decide :: Alternative f => a -> T.Tree (Decision a b) -> f b
decide x (T.Node (Action f) _) = pure (f x)
decide x (T.Node (Requirement p) subtree)
  | p x       = asum $ map (decide x) subtree
  | otherwise = empty

ifStable :: Maybe Freq -> V.Vector Freq -> MagSpect -> V.Vector Freq
ifStable f0t f0cf f0cm = case f0t of
  Nothing -> f0cf
  Just f  ->
    let shortlist = V.findIndices (\x -> abs (x - f) < f/2) f0cf
        maxc = V.maxIndex f0cm
        maxcfd = (f0cf V.! maxc) `mod` f
    in decide $
      iff (const True) [
        iff (maxcfd > f0t/2) [
          iff (notElem maxc shortlist && ((f0t - maxcfd) > (f0t / 4))) [
            action (V.backpermute f0cf (V.snoc shortlist maxc))
          ]
        ],
        iff (notElem maxc shortlist && (maxcfd > (f0t / 4))) [
          action (V.backpermute f0cf (V.snoc shortlist maxc))
        ],
        action V.backpermute f0cf shortlist
      ]

maxErr :: Double -> Freq -> Double -> Maybe Freq
maxErr errmax f0 f0err = case f0 of
  0 -> Nothing
  x -> if f0err < errmax then Just x else Nothing


-- Wraps the TWM function to check for candidates close to f0
-- Should be changed to a decision tree for complex conditional branches;
-- see: https://stackoverflow.com/a/23506203
-- Need to check if f0cf length is 0 to prematurely return 0
f0Twm :: V.Vector Freq  -- Peak Freqs
      -> MagSpect       -- Peak Mags
      -> Double         -- Error Threshold
      -> Freq           -- minimum Freq
      -> Freq           -- maximum Freq
      -> Maybe Freq     -- Last f0 Cand
      -> Maybe Freq
f0Twm pfreq pmag errmax minfreq maxfreq f0t
  | V.length pfreq < 3 && isNothing f0t = Nothing
  | V.length f0c == 0 = Nothing
  | V.length f0cf' == 0 = Nothing
  | otherwise = maxErr errmax $ twm pfreq pmag f0cf'
    where
      f0c = V.findIndices (\x -> x > minfreq && x < maxfreq) pfreq
      f0cf = V.backpermute pfreq f0c
      f0cm = V.backpermute pmag f0c
      f0cf' = ifStable f0t f0cf f0cm




-- Vector of peak (freq, mag), Vector of (freq, mag) candidates for f0
twm :: V.Vector Freq -> MagSpect -> V.Vector Freq -> (Double, Double)
twm pfreq pmag candidates = V.minimumBy (comparing snd) genErrs where
    -- Values for the Two way mismatch algorithm
    p = 0.5 :: Double
    q = 1.4
    r = 0.5
    rho = 0.33
    -- Lower number of harmonics is better for signals
    -- with a lot of noise. 10 may be ideal.
    maxnpeaks = 10
    aMax = V.maximum pmag
    fMax = V.maximum pfreq
    --    fN :: Double -> Int
    fN fund = ceiling (fMax/fund)
    -- Vector of f0 candidates and corresponding
    -- error ratio.
    genErrs :: V.Vector (Double, Double)
    genErrs = V.map (errTotal.genPartials) candidates
    genPartials f = V.iterateN (fN f) (+f) f
    errTotal vec = (V.head vec, pToM pfreq vec + rho * mToP pfq vec)

    -- Takes a vector of (freq, mag) and a vector of predicted spectrums
    -- and calculates the predicted to measured error and the length
    -- of the predicted vector = N.
    pToM :: V.Vector Freq -> MagSpect -> V.Vector Freq -> Double
    pToM pfreq pmag predicted = V.sum (V.map errpm predicted) / fromIntegral (V.length predicted) where
      errpm :: Double -> Double
      errpm pfreq = let (deltaf, freq, mag) = V.minimumBy (comparing one) $ V.map absmin vecfm
                        one (a, _, _) = a
                        absmin :: (Double, Double) -> (Double, Double, Double)
                        absmin (f, m) = (abs (pfreq - f), f, m)
                        freqp = freq ** (-p)
                     in (deltaf * freqp) + ((mag/aMax) *
                        ((q * deltaf * freqp) - r))

    -- Takes a vector of (freq, mag) and a vector of predicted spectrums
    -- and calculates the measured to predicted error and the length
    -- of the peaks vector = K.

    mToP :: V.Vector Freq -> MagSpect -> V.Vector Freq -> Double
    mToP vecfm predicted = V.sum (V.map errmp vecfm) / fromIntegral (V.length vecfm) where
      errmp :: (Double, Double) -> Double
      errmp (freq, mag) = let deltaf = V.minimum $ V.map absmin predicted
                              absmin :: Double -> Double
                              absmin pfreq = abs (freq - pfreq)
                              freqp = freq ** (-p)
                           in (deltaf * freqp) + ((mag/aMax) *
                              ((q * deltaf * freqp) - r))

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
  audio <- readWav "singing-female.wav"
  let window = hammingC 2048
      -- (sample rate, window, FFTsz, Hopsz, Thresh in DB, min freq, maxfreq, error margin)
      anal = f0Detection audio window 2048 256 (-80) 100 3000 5.0
      fst3 (_, _, a) = a
  mapM_ (print.fst3) anal
 -- writeFile "f0detect.txt" (show anal)
