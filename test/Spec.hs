 -- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import           Test.Hspec

import qualified Lib.STFTSpec as STFT
import qualified Lib.TWMSpec  as TWM
import qualified Lib.UtilSpec as Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Util" Util.spec
  describe "STFT" STFT.spec
  describe "TWM" TWM.spec
