
module Lib.UtilSpec where

import           Data.Coerce
import qualified Lib.TWM         as TWM
import qualified Lib.Util        as U
import           Test.Hspec
import           Test.QuickCheck

spec = do

  describe "U.intoToFreq" $ do
    it "converts an int to Hz" $ do
      U.intToFreq 3 `shouldBe` TWM.MkFreq 523.2511306011974

  describe "U.intToNote" $ do
    it "converts an int to a Note" $ do
      U.intToNote (-36) `shouldBe` U.A
      U.intToNote 3 `shouldBe` U.C

  describe "U.findf0" $ do
    it "returns the most common note in an audio file" $ do
      audio <- U.readWav "static/audio/singing-female.wav"
      let twm = (coerce $ TWM.twmWrapper (U.makeSignal audio))
      U.findf0 twm `shouldBe` U.Ab
