{-# LANGUAGE FlexibleContexts #-}

module TWM (

   test
 , twmWrapper
 , f0Detection

) where

import           Data.Fixed  (mod')
import           Data.Maybe
import qualified Data.Vector as V
import           STFT
import           Window

type Threshold = Int

type Indices = V.Vector Int

type Freq = Double

type Matrix a = V.Vector (V.Vector a)

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
            -> [Freq]
f0Detection (fs, sig) window fftsz hopsz thresh minfreq maxfreq errmax =
  fmap (fromMaybe 0) (tail $ scanl scanf0Twm Nothing lsVec) where
    sigstft = stft sig window fftsz hopsz
    lsVec = fmap peakMod sigstft
    ipFreqs (x, y, z) = (V.map ((fromIntegral fs *).(/ fromIntegral fftsz)) x, y, z)
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
    iPM i = let adj = sides i
                mags@(ml,mm,mr) = yVals mag adj
                iPeak = iploc i mags :: Double
                iMag = mm - 0.25*(ml - mr)*(iPeak- fromIntegral i)
                ind :: Int
                ind = floor iPeak
                -- Phase is decently incorrect.
                iPhase = linInterp (ind, phase V.! ind)
                                   (ind + 1, phase V.! ind + 1) iPeak
             in (iPeak, iMag, iPhase)


ifStable :: Maybe Freq -> V.Vector Freq -> MagSpect -> V.Vector Freq
ifStable f0t f0cf f0cm = case f0t of
  Nothing -> f0cf
  Just f  ->
    let shortlist = V.findIndices (\x -> abs (x - f) < f/2) f0cf
        maxc = V.maxIndex f0cm
        maxcfd = (f0cf V.! maxc) `mod'` f
    in  if maxcfd > f/2 then
          if notElem maxc shortlist && (f - maxcfd) > (f / 4) then
            V.backpermute f0cf (V.snoc shortlist maxc)
          else V.backpermute f0cf shortlist
        else
          if notElem maxc shortlist && maxcfd > (f / 4) then
            V.backpermute f0cf (V.snoc shortlist maxc)
          else V.backpermute f0cf shortlist


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
  | otherwise = uncurry (maxErr errmax) $ twm pfreq pmag f0cf'
  where
      f0c = V.findIndices (\x -> x > minfreq && x < maxfreq) pfreq
      f0cf = V.backpermute pfreq f0c
      f0cm = V.backpermute pmag f0c
      f0cf' = ifStable f0t f0cf f0cm


mZip :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
mZip f = V.zipWith (V.zipWith f)

mSub, mAdd, mMult :: Num a => Matrix a -> Matrix a -> Matrix a
mSub = mZip (-)
mAdd = mZip (+)
mMult = mZip (*)

vSub, vAdd, vMult :: Num a => V.Vector a -> V.Vector a -> V.Vector a
vSub = V.zipWith (-)
vAdd = V.zipWith (+)
vMult = V.zipWith (*)

mMap :: (a -> b) -> Matrix a -> Matrix b
mMap f = V.map (V.map f)

toBool :: Num b => (a -> Bool) -> V.Vector a -> V.Vector b
toBool p = V.map (\x -> if p x then 1 else 0)

-- Vector of peak (freq, mag), Vector of (freq, mag) candidates for f0
twm :: V.Vector Freq -> MagSpect -> V.Vector Freq -> (Double, Double)
twm pfreq pmag f0c = (f0c V.! f0index, totalError V.! f0index) where
    -- Values for the Two way mismatch algorithm
    p = 0.5
    q = 1.4
    r = 0.5
    rho = 0.33
    -- Lower number of harmonics is better for signals with a lot of noise.
    -- 10 may be ideal.
    maxnpeaks = 10 :: Int
    errorPM :: V.Vector Double
    errorPM = V.replicate (V.length f0c) 0
    maxNPM = min maxnpeaks (V.length pfreq)

    totalError :: V.Vector Double
    totalError = V.map (/ fromIntegral maxNPM) pToM `vAdd`
                 V.map (\x-> (rho * x) / fromIntegral maxNPM) mToP
    f0index = V.minIndex totalError

    magFactor = V.map (\x -> 10**((x - V.maximum pmag)/20))
    pondMag mf pd = pd `vAdd` (mf `vMult` V.map (\x -> q * x - r) pd)

    pToM :: V.Vector Double
    pToM = go maxNPM f0c errorPM where
      go 0 _ err = err
      go x h err =
        let difmatrixPM = V.map (V.replicate (V.length pfreq)) h
            difmatrixPM' = mMap abs (difmatrixPM `mSub` V.replicate (V.length h) pfreq)
            freqDistance = V.map V.minimum difmatrixPM'
            peakloc = V.map V.minIndex difmatrixPM'
            ponddif = freqDistance `vMult` V.map (**(-p)) h
            magFactor' = magFactor $ V.backpermute pmag peakloc
            err' = err `vAdd` pondMag magFactor' ponddif
        in  go (x - 1) (h `vAdd` f0c) err'

    mToP :: V.Vector Double
    mToP = go 0 V.empty where
      pfreq' = V.take maxNPM pfreq
      magFactor' = magFactor $ V.take maxNPM pmag
      go i err
        | i >= maxNPM = err
        | otherwise =
          let nharm :: V.Vector Int
              nharm = V.map (round. (/ f0c V.! i)) pfreq'
              nharm' = (toBool (>=1) nharm `vMult` nharm) `vAdd` toBool (<1) nharm
              freqDistance = V.map abs (pfreq' `vSub` V.map ((* f0c V.! i ). fromIntegral) nharm')
              ponddif = freqDistance `vMult` V.map (**(-p)) pfreq'
              err' = V.snoc err (V.sum (magFactor' `vMult` pondMag magFactor' ponddif))
         in   go (i + 1) err'

twmWrapper :: (Int, Signal) -> [Freq]
twmWrapper audio =
  let window = hammingC 2048
  -- (sample rate, window, FFTsz, Hopsz, Thresh in DB, min freq, maxfreq, error margin)
  in  f0Detection audio window 2048 256 (-80) 100 3000 5.0


test :: IO ()
test = do
  audio <- readWav "src/singing-female.wav"
  print $ twmWrapper audio
