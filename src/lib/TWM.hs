{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Lib.TWM where

import qualified Data.Coerce  as Co
import           Data.Fixed   (mod')
import qualified Data.Maybe   as M
import qualified Data.Vector  as V
import           GHC.TypeNats
import qualified Lib.STFT     as STFT

newtype Freq = MkFreq Double deriving (Eq, Num, Fractional, Ord, Real, Floating, RealFrac, Show)

newtype Error = Error Double deriving (Eq, Ord, Num, Fractional, Show)

type Matrix a = V.Vector (V.Vector a)

data MinMax = MinMax Freq Freq

data HyperParams = MkParams {
  q        :: Double,
  p        :: Double,
  r        :: Double,
  rho      :: Error,
  numHarms :: Int
}

data Config = MkConfig {
  amplitudeThreshhold :: Int,
  maxError            :: Error,
  minMax              :: MinMax,
  hyperParams         :: HyperParams
}

hyperParamC :: HyperParams
hyperParamC = MkParams {
      p = 0.5,
      q = 1.4,
      r = 0.5,
      rho = 0.33,
      numHarms = 10
    }

config :: Config
config = MkConfig {
    amplitudeThreshhold = -80,
    maxError = Error 5.0,
    minMax = MinMax (MkFreq 100) (MkFreq 3000),
    hyperParams = hyperParamC
}



uwFreqVec :: V.Vector Freq -> V.Vector Double
uwFreqVec = Co.coerce

-- linear interpolation between two points
linInterp :: (Int, Double) -> (Int, Double) -> Double -> Double
linInterp (x0, y0) (x1, y1) x = y0 + (x - fromIntegral x0)*((y1 - y0)/ fromIntegral (x1 - x0))

-- |Takes a (sampling rate, Signal), a window function, an FFTsz, A Hopsz,
-- minimum frequency, maximum frequency, A Threshold for amplitude,
-- maximum error threshold and returns a list of f0 candidates
f0Detection :: forall h w f. (KnownNat h, KnownNat w, KnownNat f) => Config
            -> STFT.Config h w f
            -> (Int, STFT.Signal)
            -> [Freq]
f0Detection twmConf@(MkConfig ampthresh _ _ _) stftConf@(STFT.MkConfig _ _ fftsz) (fs, sig) =
  fmap (M.fromMaybe 0) (tail $ scanl scanf0Twm Nothing lsVec) where
    spectrums = STFT.stft stftConf sig
    lsVec = fmap peakMod spectrums

    -- |Converts the vector of interpolated indices to frequencies
    ipFreqs :: (V.Vector Double, a) -> (V.Vector Freq, a)
    ipFreqs (x, y) = (Co.coerce $ V.map ((fromIntegral fs *).(/ fromIntegral (natVal fftsz))) x, y)

    scanf0Twm :: Maybe Freq -> (V.Vector Freq, STFT.Spectrums) -> Maybe Freq
    scanf0Twm f0cand (ipfreq, STFT.MkSpect (ipmag, _)) = f0Twm twmConf ipfreq ipmag f0cand

    peakMod :: STFT.Spectrums -> (V.Vector Freq, STFT.Spectrums)
    peakMod spectrum = ipFreqs $ peakInterp spectrum (peakDetection ampthresh spectrum)


-- |Takes a minimum threshold and a vector of magnitudes, and returns
-- a vector of indices of local maxima above the threshold.
peakDetection :: Int -> STFT.Spectrums -> V.Vector Int
peakDetection thresh (STFT.MkSpect (STFT.MkMag vec, _)) = V.map (+1) (V.findIndices (/=0) ploc) where
  indpeaks = V.tail $ V.init $ V.indexed vec
  threshVec = V.map (filtTo0 (fromIntegral thresh) . snd) indpeaks
  next = V.map fmajor indpeaks
  prev = V.map fminor indpeaks
  fmajor (ind, val) = filtTo0 (vec V.! (ind+1)) val
  fminor (ind, val) = filtTo0 (vec V.! (ind-1)) val
  filtTo0 :: Double -> Double -> Double
  filtTo0 t x = if x > t then x else 0
  ploc = V.zipWith3 (\x y z -> x * y * z) threshVec next prev

-- Refactor using scan
--
-- peakDetection :: Int -> STFT.Spectrums -> V.Vector Int
-- peakDetection thresh (STFT.MkSpect (STFT.MkMag magVec, _)) = V.map (+1) peakIndices where
--   vec = V.tail $ V.init $ magVec
--   above = V.map (filtTo0 (fromIntegral thresh)) vec
--   next = V.scanl1 filtTo0 vec
--   prev = V.scanr1 filtTo0 vec

--   filtTo0 t x = if x > t then x else 0
--   nonZeroPeaks = V.zipWith3 (\x y z -> x * y * z) above next prev
--   peakIndices = (V.findIndices (/=0) nonZeroPeaks)


-- |Takes the magntiude and phase spectrum, and a vector of indices of the peak magintudes,
-- and returns a vector of interpolated locations and magnitudes for the peaks
peakInterp :: STFT.Spectrums -> V.Vector Int -> (V.Vector Double, STFT.Spectrums)
peakInterp (STFT.MkSpect (STFT.MkMag mag, STFT.MkPhase phase)) peaks =
  (locations, STFT.MkSpect (STFT.MkMag magresult, STFT.MkPhase phaseresult)) where
    sides ind = (ind - 1, ind, ind + 1)
    yVals vec (i1, i2, i3) = (vec V.! i1, vec V.! i2, vec V.! i3)
    iploc p (l, m, r) = fromIntegral p + 0.5 * (l - r)/(l - 2 * m + r)
    iPM i = let adj = sides i
                mags@(ml,mm,mr) = yVals mag adj
                iPeak = iploc i mags :: Double
                iMag = mm - 0.25*(ml - mr)*(iPeak- fromIntegral i)
                ind = floor iPeak :: Int
                -- ^Phase is decently incorrect.
                iPhase = linInterp (ind, phase V.! ind)
                                   (ind + 1, phase V.! ind + 1) iPeak
             in (iPeak, iMag, iPhase)
    (locations, magresult, phaseresult) = V.unzip3 (V.map iPM peaks)


ifStable :: Maybe Freq -> V.Vector Freq -> STFT.MagnitudeSpectrum -> V.Vector Freq
ifStable f0cand peakFreqs (STFT.MkMag peakMags) = case f0cand of
  Nothing -> peakFreqs
  Just f  ->
    let shortlist = V.findIndices (\x -> abs (x - f) < f / 2) peakFreqs
        maxc = V.maxIndex peakMags
        maxcfd = (peakFreqs V.! maxc) `mod'` f
    in  if maxcfd > f/2 then
          if notElem maxc shortlist && (f - maxcfd) > (f / 4) then
            V.backpermute peakFreqs (V.snoc shortlist maxc)
          else V.backpermute peakFreqs shortlist
        else
          if notElem maxc shortlist && maxcfd > (f / 4) then
            V.backpermute peakFreqs (V.snoc shortlist maxc)
          else V.backpermute peakFreqs shortlist


maxErr :: Error -> (Freq, Error) -> Maybe Freq
maxErr errmax (f0cand, f0err) = case f0cand of
  0 -> Nothing
  x -> if f0err < errmax then Just x else Nothing


-- |Wraps the TWM function to check for candidates close to f0
-- Should be changed to a decision tree for complex conditional branches;
-- see: https://stackoverflow.com/a/23506203
-- Need to check if f0cf length is 0 to prematurely return 0
f0Twm :: Config
      -> V.Vector Freq            -- Peak Freqs
      -> STFT.MagnitudeSpectrum   -- Peak Mags
      -> Maybe Freq               -- Last f0 Cand
      -> Maybe Freq
f0Twm (MkConfig _ errmax (MinMax minfreq maxfreq) hparams) pfreq pMag@(STFT.MkMag vecMag) f0cand
  | V.length pfreq < 3 && M.isNothing f0cand = Nothing
  | V.length f0withinRanges == 0 = Nothing
  | V.length f0cf' == 0 = Nothing
  | otherwise = maxErr errmax $ twm hparams pfreq pMag f0cf'
  where
      f0withinRanges = V.findIndices (\x -> x > minfreq && x < maxfreq) pfreq
      f0peakFreqs = V.backpermute pfreq f0withinRanges
      f0peakMags = STFT.MkMag $ V.backpermute vecMag f0withinRanges
      f0cf' = ifStable f0cand f0peakFreqs f0peakMags


mZip :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
mZip f = V.zipWith (V.zipWith f)

mSub :: Num a => Matrix a -> Matrix a -> Matrix a
mSub = mZip (-)

vSub, vAdd, vMult :: Num a => V.Vector a -> V.Vector a -> V.Vector a
vSub = V.zipWith (-)
vAdd = V.zipWith (+)
vMult = V.zipWith (*)

mMap :: (a -> b) -> Matrix a -> Matrix b
mMap f = V.map (V.map f)

toBool :: Num b => (a -> Bool) -> V.Vector a -> V.Vector b
toBool predicate = V.map (\x -> if predicate x then 1 else 0)

-- |Vector of the peak frequencies, vector of peak magnitudes, and a vector
-- of the previous fundamental frequency (f0) candidates and returns a
-- tuple of the f0 candidate and its respective error.
twm :: HyperParams -> V.Vector Freq -> STFT.MagnitudeSpectrum -> V.Vector Freq -> (Freq, Error)
twm hparams pfreq (STFT.MkMag pmag) f0cands = (f0cands V.! f0index, totalError V.! f0index) where
    p' = p hparams
    q' = q hparams
    r' = r hparams
    rho' = rho hparams
    -- ^Lower number of harmonics is better for signals with a lot of noise.
    -- 10 may be ideal.
    maxnpeaks = numHarms hparams

    -- ^Initialize of vector of errors
    errorPM :: V.Vector Error
    errorPM = V.replicate (V.length f0cands) 0

    -- ^Take the minimum of either your peak frequencies or maximum number of harmoonics
    maxNPM = min maxnpeaks (V.length pfreq)

    getTotalError :: V.Vector Error -> V.Vector Error -> V.Vector Error
    getTotalError pToM mToP = V.map (/ fromIntegral maxNPM) pToM `vAdd`
      V.map (\x-> (rho' * x) / fromIntegral maxNPM) mToP

    -- ^Get the index with the least amount of error
    totalError = getTotalError predictedToMeasured measuredToPredicted

    f0index = V.minIndex totalError

    magFactor :: V.Vector Double -> V.Vector Double
    magFactor = V.map (\x -> 10**((x - V.maximum pmag) / 20))

    pondMag :: V.Vector Double -> V.Vector Double -> V.Vector Double
    pondMag mf pd = pd `vAdd` (mf `vMult` V.map (\x -> q' * x - r') pd)

    predictedToMeasured :: V.Vector Error
    predictedToMeasured = go maxNPM f0cands errorPM where
      go :: Int -> V.Vector Freq -> V.Vector Error -> V.Vector Error
      go 0 _ err = err
      go x h err =
        let difmatrixPM = V.map (V.replicate (V.length pfreq)) h
            difmatrixPM' = mMap abs (difmatrixPM `mSub` V.replicate (V.length h) pfreq)
            freqDistance = V.map V.minimum difmatrixPM'
            peakloc = V.map V.minIndex difmatrixPM'
            ponddif = (uwFreqVec freqDistance) `vMult` V.map (**(-p')) (uwFreqVec h)
            magFactor' = magFactor $ V.backpermute pmag peakloc
            err' = err `vAdd` (Co.coerce $ pondMag magFactor' ponddif)
        in  go (x - 1) (h `vAdd` f0cands) err'

    measuredToPredicted :: V.Vector Error
    measuredToPredicted = go 0 V.empty where
      pfreq' = V.take maxNPM pfreq
      magFactor' = magFactor $ V.take maxNPM pmag
      go :: Int -> V.Vector Error -> V.Vector Error
      go i err
        | i >= maxNPM = err
        | otherwise =
          let nharm :: V.Vector Int
              nharm = V.map (round . (/ f0cands V.! i)) pfreq'
              nharm' = (toBool (>=1) nharm `vMult` nharm) `vAdd` toBool (<1) nharm
              freqDistance = V.map abs (pfreq' `vSub` V.map ((* f0cands V.! i ). fromIntegral) nharm')
              ponddif = (uwFreqVec freqDistance) `vMult` V.map (** (- p')) (uwFreqVec pfreq')
              err' = V.snoc err (Co.coerce $ V.sum (magFactor' `vMult` pondMag magFactor' ponddif))
         in   go (i + 1) err'

twmWrapper :: (Int, STFT.Signal) -> [Freq]
twmWrapper = f0Detection config STFT.config

