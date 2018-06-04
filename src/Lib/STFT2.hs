{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeApplications           #-}

{-
A lot of error handling needs to be added to the code. For example
0 < hopsize < windowsize < fftsize needs to be reinforced.
-}

module Lib.STFT2 where

import qualified Data.Complex                  as C
import           Data.Proxy
import           Data.Type.Equality
import qualified Data.Vector                   as V
import           GHC.TypeNats
import           Numeric.FFT.Vector.Invertible

-- A small number
epsilon :: Double
epsilon = 2.2204460492503131e-16

newtype MagnitudeSpectrum = MkMag (V.Vector Double)

newtype PhaseSpectrum = MkPhase (V.Vector Double)

newtype Signal = MkSignal (V.Vector (C.Complex Double))

newtype Window = MkWindow (V.Vector (C.Complex Double))

type family IsPo2 (m :: Nat) :: Bool where
  IsPo2 n = 2 ^ Log2 n == n

data FFTsz n where
  MkFFTsz :: (KnownNat n, IsPo2 n ~ 'True) => Proxy n -> FFTsz n

data Winsz n where
  MkWinsz :: KnownNat n => Proxy n -> Winsz n

data Hopsz n where
  MkHopsz :: KnownNat n => Proxy n -> Hopsz n

data Config h w f where
  MkConfig :: forall h w f. (KnownNat f, KnownNat w, KnownNat h, h <= w, w <= f) => Hopsz h -> Winsz w -> FFTsz f -> Config h w f

hamming :: forall n. KnownNat n => Winsz n -> Window
hamming _ = MkWindow $ V.generate size goHamm where
  size = fromIntegral $ natVal (Proxy @n) :: Int
  goHamm :: Int -> C.Complex Double
  goHamm i = (C.:+ 0) $ 0.54 - 0.46 * cos (2 * pi * fromIntegral i / (fromIntegral size - 1))

-- Phase unwrapping algorithm.
-- Converting to and from list for pattern matching is a little cumbersome.
unwrapPhase :: PhaseSpectrum -> PhaseSpectrum
unwrapPhase (MkPhase xs) = MkPhase $ V.fromList $ (diff (V.toList xs) 0) where
  diff (x:y:ys) accm
    | (y+accm) - x > pi    = x: diff (y+accm-(2*pi):ys) (accm-(2*pi))
    | (y+accm) - x < (-pi) = x: diff (y+accm+(2*pi):ys) (accm+(2*pi))
    | otherwise             = x: diff (y+accm:ys) accm
  diff [x] _     = [x]
  diff []  _     = []

halfInt, halfIntPlus :: Int -> Int
halfIntPlus win = (win + 1) `div` 2
halfInt win = win `div` 2

-- Takes a complex signal and fftsz does zero phase windows
-- https://ccrma.stanford.edu/~jos/sasp/Zero_Phase_Zero_Padding.html
zeroPhaseZeroPad :: forall n. KnownNat n => FFTsz n -> Signal -> Signal
zeroPhaseZeroPad _ (MkSignal sig) = MkSignal $ positive V.++ zeros V.++ negative where
  fftsz = fromIntegral $ natVal (Proxy @n)
  win = V.length sig
  halfSig = halfInt win
  halfSigPlus = halfIntPlus win
  positive = V.slice halfSig halfSigPlus sig
  negative = V.slice 0 halfSig sig
  zeros = V.replicate (fftsz - halfSig - halfSigPlus) (0 C.:+ 0)

nearZero :: Double -> Bool
nearZero a = abs a <= 1e-12

-- Calculates the magnitude spectrum in Db
magSpect :: Signal -> MagnitudeSpectrum 
magSpect (MkSignal sig) = MkMag $ V.map (dB. near0 . C.magnitude) sig where
  dB x = 20 * logBase 10 x
  near0 x = if nearZero x then epsilon else x


-- Calculates the phase spectrum
phaseSpect :: Signal -> PhaseSpectrum 
phaseSpect (MkSignal vec) = unwrapPhase $ MkPhase $ V.map (C.phase . to0) vec where
  to0 x = go (C.realPart x) (C.imagPart x)
  go x y | nearZero x && nearZero y = 0 C.:+ 0
         | nearZero x               = 0 C.:+ y
         | nearZero y               = x C.:+ 0
         | otherwise                = x C.:+ y

-- Takes a windowed signal and FFT size returns a vector of tuples
-- (Magnitude, Phase). Magnitude in Db, phase unwrapped
dftAnal :: forall n. KnownNat n => FFTsz n -> Signal -> (MagnitudeSpectrum, PhaseSpectrum)
dftAnal fftsz sig = (magSpect trsf, phaseSpect trsf) where
    (MkSignal inputVec) = zeroPhaseZeroPad fftsz sig
    trsf = MkSignal $ run dft inputVec

-- Statically guaruntee that chunksize is larger than hop sz
divvy :: forall n m a. (KnownNat n, KnownNat m) => Winsz n 
          -> Hopsz m -> V.Vector a -> [V.Vector a]
divvy _ _ = go n' m' where
  n' = fromIntegral $ natVal (Proxy :: Proxy n)
  m' = fromIntegral $ natVal (Proxy :: Proxy m)

  go :: Int -> Int -> V.Vector a -> [V.Vector a]
  go x y vec
    | V.length vec <= x = [vec]
    | otherwise = V.take x vec : go x y (V.drop y vec)

normalize :: Fractional a => V.Vector a -> V.Vector a
normalize win = fmap (/ V.foldl1' (+) win) win

-- Takes an input signal, a window, an fftsize and a hop size and
-- computes the short time Fourier transform, returning a list of
-- (vector of magnitude, vector of phase)
stft :: forall h w f. (KnownNat h, KnownNat w, KnownNat f) => Config h w f -> Signal 
     -> [(MagnitudeSpectrum, PhaseSpectrum)]
stft (MkConfig hopsz winsz fftsz) (MkSignal sig) = fmap winDFT splitSig where
  (MkWindow win) = hamming winsz
  halfWindow = floor (fromIntegral (natVal (Proxy @w)) / 2 :: Double)
  zs = V.replicate halfWindow (0 C.:+ 0)
  sigzs = zs V.++ sig V.++ zs
  splitSig = divvy winsz hopsz sigzs
  
  winDFT :: V.Vector (C.Complex Double) -> (MagnitudeSpectrum, PhaseSpectrum)
  winDFT vec = dftAnal fftsz (MkSignal $ V.zipWith (*) vec (normalize win))


-- Takes a tuple of vectors (Magnitude, Phase) and a window size
-- and returns the original signal
dftSynth :: forall n. KnownNat n => Winsz n 
         -> (MagnitudeSpectrum, PhaseSpectrum) -> V.Vector (C.Complex Double)
dftSynth _ (MkMag magVec, MkPhase phaseVec) =
  let win = fromIntegral $ natVal (Proxy @n)
      hwin = halfInt win
      hwinp = halfIntPlus win
      vec = V.zipWith (\x y -> (x, y)) magVec phaseVec
      posFreqs = fmap f vec where
        f (mag, phase) = (10**(mag/20) C.:+ 0) *
                         exp ((phase C.:+ 0) *
                         (0 C.:+ 1))
      negFreqs = (fmap f . V.reverse) (g vec) where
        g = (V.init . V.tail)
        f (mag, phase) = (10**(mag/20) C.:+ 0) *
                         exp ((phase C.:+ 0) *
                         (0 C.:+ (-1)))
      resultidft = run idft (posFreqs V.++ negFreqs)
  in V.slice (V.length resultidft - hwin) 
                    hwin resultidft V.++ V.take hwinp resultidft


stftSynth :: forall h w. (KnownNat h, KnownNat w) => [(MagnitudeSpectrum, PhaseSpectrum)] 
          -> Hopsz h -> Winsz w -> Signal
stftSynth magphase _ winsz =
  let hwinp = halfIntPlus $ fromIntegral $ natVal (Proxy @w)
      signalFrames = fmap (fmap (fromIntegral (natVal (Proxy @h)) * ) . dftSynth winsz ) magphase
      signalTuples = fmap (V.splitAt hwinp) signalFrames
      overlapAdd (x1, x2) (y1, y2) = (x1 V.++ V.zipWith (+) x2 y1, y2)
   in MkSignal $ V.drop hwinp $ fst (foldl overlapAdd (Prelude.head signalTuples) (Prelude.tail signalTuples))

   
