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


{-
A lot of error handling needs to be added to the code. For example
0 < hopsize < windowsize < fftsize needs to be reinforced.
-}

module Lib.STFT2 where

import qualified Data.Complex                  as C
import qualified Data.Finite                   as F
import           Data.Maybe
import           Data.Proxy
import           Data.Type.Equality
import qualified Data.Vector                   as V
import qualified Data.Vector.Sized             as S
import           GHC.TypeNats
import           Numeric.FFT.Vector.Invertible

newtype Window n a = Window {
  mkWindow :: S.Vector n a
} deriving (Eq, Ord, Show, Num, Functor, Semigroup, Monoid)

-- A hamming window of size n.
hamming :: forall n p. KnownNat n => p n -> Window n Double
hamming proxy = Window $ S.generate' proxy goHamm where
  size = natVal proxy
  goHamm :: F.Finite n -> Double
  goHamm findex =
    let index = F.getFinite findex
     in 0.54 - 0.46 * cos (2 * pi * fromIntegral index / (fromIntegral size - 1))


hammingComplex :: forall n p. KnownNat n => p n -> Window n (C.Complex Double)
hammingComplex proxy = Window $ S.generate' proxy goHamm where
  size = natVal proxy
  goHamm :: F.Finite n -> C.Complex Double
  goHamm findex =
    let index = F.getFinite findex
     in (C.:+ 0) 0.54 - 0.46 * cos (2 * pi * fromIntegral index / (fromIntegral size - 1))

-- A small number
epsilon :: Double
epsilon = 2.2204460492503131e-16

newtype MagnitudeSpectrum n = Mag {
  mkMag :: S.Vector n Double
}

newtype PhaseSpectrum n = Phase {
  mkPhase :: S.Vector n Double
}

type family IsPo2 (m :: Nat) :: Bool where
  IsPo2 n = 2 ^ Log2 n == n

data FFTsz n where
  MkFFTsz :: (KnownNat n, IsPo2 n ~ 'True) => Proxy n -> FFTsz n

newtype Winsz n = Winsz {
  mkWinsz :: F.Finite n
}

newtype Hopsz n = Hopsz {
  mkHopsz :: F.Finite n
}

newtype Signal n = Signal {
  mkSignal :: S.Vector n (C.Complex Double)
}

-- This should never throw, but could lead to async exceptions in worst case
unsafeToSized :: forall p n a. KnownNat n => p n -> V.Vector a -> S.Vector n a
unsafeToSized _ vec = fromMaybe (error "Proxy arguement does not match vector length")
  (S.toSized vec :: Maybe (S.Vector n a))

-- Phase unwrapping algorithm.
-- Converting to and from list for pattern matching is a little cumbersome.
unwrapPhase :: forall n. KnownNat n => PhaseSpectrum n -> PhaseSpectrum n
unwrapPhase (Phase xs) = Phase $ fromJust $ S.fromListN' (Proxy :: Proxy n) (diff (S.toList xs) 0) where
  diff (x:y:ys) accm
    | (y+accm) - x > pi    = x: diff (y+accm-(2*pi):ys) (accm-(2*pi))
    | (y+accm) - x < (-pi) = x: diff (y+accm+(2*pi):ys) (accm+(2*pi))
    | otherwise             = x: diff (y+accm:ys) accm
  diff [x] _     = [x]
  diff []  _     = []

halfInt, halfIntPlus :: Int -> Int
halfIntPlus win = (win + 1) `div` 2
halfInt win = win `div` 2

halfNat, halfNatPlus :: forall p n. (KnownNat n) => p n -> Int
halfNatPlus _ = fromIntegral (natVal (Proxy :: Proxy n) + 1) `div` 2
halfNat _ = fromIntegral (natVal (Proxy :: Proxy n)) `div` 2


halfNat' :: forall p n . KnownNat n => p n -> Proxy (Div n 2)
halfNat' _ = Proxy :: Proxy (Div n 2)


halfNatPlus' :: forall p n. KnownNat n => p n -> Proxy (Div (n + 1) 2)
halfNatPlus' _ = Proxy :: Proxy (Div (n +1) 2)

type family FloorBy2 (n :: Nat) :: Nat where
  FloorBy2 n = Div n 2

type family CeilingBy2 (n :: Nat) :: Nat where
  CeilingBy2 n = Div (n + 1) 2


-- Takes a complex signal and fftsz does zero phase zero padding
-- https://ccrma.stanford.edu/~jos/sasp/Zero_Phase_Zero_Padding.html
zeroPhaseZeroPad :: forall n m. (KnownNat n, KnownNat m, n <= m) => FFTsz m -> Signal n -> Signal m
zeroPhaseZeroPad _ (Signal sig) = Signal $ unsafeToSized (Proxy :: Proxy m) zeroPadded where
  fftsz = fromIntegral $ natVal (Proxy :: Proxy m)
  halfSig = halfNat (Proxy :: Proxy n)
  halfSigPlus = halfNatPlus (Proxy :: Proxy n)
  positive = V.slice halfSig halfSigPlus (S.fromSized sig)
  negative = V.slice 0 halfSig (S.fromSized sig)
  zs = V.replicate (fftsz - halfSig - halfSigPlus) (0 C.:+ 0)
  zeroPadded = positive V.++ zs V.++ negative

{-
-- Must prove to the compiler that n ~ (Div n 2 + Div (n + 1) 2)
zeroPhaseZeroPad' :: forall n m. (KnownNat n, KnownNat m, n <= m) => FFTsz m -> Signal n -> Signal m
zeroPhaseZeroPad' _ (Signal sig) = Signal $ positive S.++ zs S.++ negative where
  halfSig = halfNat' (Proxy :: Proxy n)
  halfSigPlus = halfNatPlus' (Proxy :: Proxy n)
  positive :: S.Vector (Div (n + 1) 2) (C.Complex Double)
  positive = S.slice' halfSig halfSigPlus (sig :: S.Vector (Div n 2 + Div (n + 1) 2) (C.Complex Double))
  negative :: S.Vector (Div n 2) (C.Complex Double)
  negative = S.slice' (Proxy :: Proxy 0) halfSig sig
  zs = S.replicate' (Proxy :: Proxy (m - n)) (0 C.:+ 0)
-}

nearZero :: Double -> Bool
nearZero a = abs a <= 1e-12

-- Calculates the magnitude spectrum in Db
magSpect :: forall n. KnownNat n => Signal n -> MagnitudeSpectrum n
magSpect (Signal sig) = Mag $ S.map (dB. near0 . C.magnitude) sig where
  dB x = 20 * logBase 10 x
  near0 x = if nearZero x then epsilon else x


-- Calculates the phase spectrum
phaseSpect :: forall n. KnownNat n => Signal n -> PhaseSpectrum n
phaseSpect (Signal vec) = unwrapPhase $ Phase $ S.map (C.phase . to0) vec where
  to0 x = go (C.realPart x) (C.imagPart x)
  go x y | nearZero x && nearZero y = 0 C.:+ 0
         | nearZero x               = 0 C.:+ y
         | nearZero y               = x C.:+ 0
         | otherwise                = x C.:+ y


-- Takes a windowed signal and FFT size returns a vector of tuples
-- (Magnitude, Phase). Magnitude in Db, phase unwrapped
dftAnal :: forall n m. (KnownNat n, KnownNat m, n <= m) => FFTsz m
        -> Signal n -> (MagnitudeSpectrum m, PhaseSpectrum m)
dftAnal fftsz sig = (magSpect trsf, phaseSpect trsf) where
    (Signal inputVec) = zeroPhaseZeroPad fftsz sig
    trsf = Signal $ unsafeToSized (Proxy :: Proxy m)
      (run dft $ S.fromSized inputVec)


-- Statically guaruntee that fftsz is larger than hop sz
divvyVec :: forall n m a. (KnownNat n, KnownNat m, m <= n) => FFTsz n
          -> Hopsz m -> V.Vector a -> [V.Vector a]
divvyVec _ _ = go n' m' where
  n' = fromIntegral $ natVal (Proxy :: Proxy n)
  m' = fromIntegral $ natVal (Proxy :: Proxy m)

  go :: Int -> Int -> V.Vector a -> [V.Vector a]
  go x y vec
    | V.length vec <= x = []
    | otherwise = V.take x vec : go x y (V.drop y vec)



-- Takes an input signal, a window, an fftsize and a hop size and
-- computes the short time Fourier transform, returning a list of
-- (vector of magnitude, vector of phase)
stft :: forall f h w s. (KnownNat f, KnownNat h, KnownNat w, KnownNat s, h <= w, w <= f, f <= s) => Window w Double
     -> FFTsz f -> Hopsz h -> Signal s -> [(MagnitudeSpectrum f, PhaseSpectrum f)]
stft (Window win) fftsz hopsz (Signal sig) = fmap winDFT splitSig where
  winsz = S.length' win
  w = fmap ((C.:+ 0) . (/ S.foldl1' (+) win)) win  -- Normalize window function
  halfWin = floor (fromIntegral winsz / 2 :: Double)
  zs = S.replicate' (halfNat' winsz) (0 C.:+ 0)
  sigzs = zs S.++ sig S.++ zs
  splitSig = divvyVec fftsz hopsz sigzs

  winDFT :: Signal f -> (MagnitudeSpectrum f, PhaseSpectrum f)
  winDFT (Signal vec) = dftAnal fftsz (Signal (vec * w))


{-
-- Takes a vector of tuples (Magnitude, Phase) and a window size
-- and returns the original signal
dftSynth :: Winsz -> (MagSpect, PhaseSpect) -> Signal
dftSynth win (magVec, phaseVec) =
  let vec = V.zipWith (\x y -> (x, y)) magVec phaseVec
      posFreqs = V.map f vec where
        f (mag, phase) = (10**(mag/20) C.:+ 0) *
                         exp ((phase C.:+ 0) *
                         (0 C.:+ 1))
      negFreqs = (V.map f . V.reverse) (g vec) where
        g = (V.init . V.tail)
        f (mag, phase) = (10**(mag/20) C.:+ 0) *
                         exp ((phase C.:+ 0) *
                         (0 C.:+ (-1)))
      resultidft = run idft (posFreqs V.++ negFreqs)
  in V.map C.realPart $
     V.concat [V.slice (V.length resultidft - hM2 win) (hM2 win) resultidft
              , V.take (hM1 win) resultidft]


stftSynth :: [(MagSpect, PhaseSpect)] -> Hopsz -> Winsz -> Signal
stftSynth magphase hopsz winsz =
  let signalFrames = fmap (V.map (fromIntegral hopsz * ) . dftSynth winsz ) magphase
      signalTuples = fmap (V.splitAt $ hM1 winsz) signalFrames
      overlapAdd (x1, x2) (y1, y2) = (x1 V.++ V.zipWith (+) x2 y1, y2)
   in V.drop (hM1 winsz) $ fst (L.foldl' overlapAdd
                                        (Prelude.head signalTuples) (Prelude.tail signalTuples))
-}
