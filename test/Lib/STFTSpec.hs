{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Lib.STFTSpec where

import           Data.Coerce
import           Data.Complex
import           Data.Proxy
import qualified Data.Vector     as V
import           GHC.TypeNats
import qualified Lib.STFT        as STFT
import qualified Lib.Util        as U
import           Test.Hspec
import           Test.QuickCheck

takeLast :: Int -> V.Vector a -> V.Vector a
takeLast n vec = V.reverse $ V.take n $ V.reverse vec

spec = do

  describe "STFT.hamming" $ do
    it "creates a hamming window of size N" $ do
      let result = V.fromList [8.000000000000002e-2 :+ 0.0,
              0.54 :+ 0.0,1.0 :+ 0.0,
              0.5400000000000001 :+ 0.0,
              8.000000000000002e-2 :+ 0.0
            ]
      STFT.runWindow STFT.hamming (STFT.MkWinsz (Proxy @5)) `shouldBe` result

  describe "STFT.zeroPhaseZeroPad" $ do
    it "peforms zero phase and zero padding" $ do
      tuple <- U.readWav "static/audio/singing-female.wav"
      let firstFive = V.fromList [(-0.553192138671875) :+ 0.0,
              (-0.55218505859375) :+ 0.0,
              (-0.556488037109375) :+ 0.0,
              (-0.58892822265625) :+ 0.0,
              (-0.610992431640625) :+ 0.0
            ]
          lastFive = V.fromList [(-0.443511962890625) :+ 0.0,
              (-0.479644775390625) :+ 0.0,
              (-0.49029541015625) :+ 0.0,
              (-0.493133544921875) :+ 0.0,
              (-0.52313232421875) :+ 0.0
            ]
          (_, audio) = U.makeSignal tuple
          result :: V.Vector (Complex Double)
          result = coerce $ STFT.zeroPhaseZeroPad (STFT.MkFFTsz (Proxy @2048)) audio
      V.take 5 result `shouldBe` firstFive
      takeLast 5 result `shouldBe` lastFive

  describe "STFT.dftAnal" $ do
    it "performs a dft of the given FFT size" $ do
      tuple <- U.readWav "static/audio/singing-female.wav"
      let firstFive = V.fromList [-12.049319770407699,
              -3.8232231987269523,
              -3.5161910098012816,
              -5.778847744891994,
              -4.450941161692169
            ]
          lastFive = V.fromList [9.085997084736617,
              12.384474935554943,
              11.667317158981483,
              20.354458129771224,
              11.951847223595225
            ]
          (_, audio) = U.makeSignal tuple
          (STFT.MkSpect (STFT.MkMag mag, _)) = STFT.dftAnal (STFT.MkFFTsz (Proxy @2048)) audio
      V.take 5 mag `shouldBe` firstFive
      takeLast 5 mag `shouldBe` lastFive

  describe "STFT.stft" $ do
    it "Performs the short time fourier transform" $ do
      (_, audio) <- U.readWav "static/audio/singing-female.wav"
      let firstFive = V.fromList [-95.55314985231192,
              -75.57780027962905,
              -72.63091749228488,
              -74.5060313038982,
              -80.35599835805851
            ]
          lastFive = V.fromList [-132.57867497788396,
              -128.76434452701437,
              -128.15973988408462,
              -131.54917075905306,
              -140.28246959896572
            ]
          (STFT.MkSpect (STFT.MkMag mag, _)) = head $ STFT.stft STFT.config (U.makeSig audio)
      V.take 5 mag `shouldBe` firstFive
      takeLast 5 mag `shouldBe` lastFive

  describe "STFT.divvy" $ do
    it "Splits the vector into " $ do
      let vec = V.replicate 272243 ()
          divvyVec = STFT.divvy (STFT.MkWinsz (Proxy @2048)) (STFT.MkHopsz (Proxy @256)) vec
      fmap length divvyVec `shouldBe` replicate 1056 2048
