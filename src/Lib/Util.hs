{-# LANGUAGE OverloadedStrings #-}

module Lib.Util where

import           Control.Arrow
import           Data.Aeson
import qualified Data.Complex           as C
import           Data.List              (foldl')
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import qualified Data.Vector            as V
import qualified Data.WAVE              as WAV
import           Database.Persist.Class
import           Database.Persist.Sql
import qualified Lib.STFT               as STFT
import qualified Lib.TWM                as TWM

-- Takes a vector of doubles, the sample frequency, and a name
-- and writes the audio file. Written in 32 bit.
writeWav :: STFT.Signal -> Int -> String -> IO ()
writeWav (STFT.MkSignal vec) sf name =
  let samples = fmap ((:[]) . WAV.doubleToSample . C.realPart) (V.toList vec)
      header = WAV.WAVEHeader 1 sf 32 Nothing
  in  WAV.putWAVEFile name (WAV.WAVE header samples)


-- Takes a Path and returns IO (sampling frequency, Vector signal).
readWav :: String -> IO (Int, V.Vector Double)
readWav path = do
  audio <- WAV.getWAVEFile path
  let header = WAV.waveHeader audio
      samples = WAV.waveSamples audio
      channels = WAV.waveNumChannels header
      sampRate = WAV.waveFrameRate header
  case channels of
    1 -> let sig = V.fromList $ fmap (WAV.sampleToDouble . Prelude.head) samples
          in return (sampRate, sig)
    _ -> error "Should be mono."

makeSignal :: (Int, V.Vector Double) -> (Int, STFT.Signal)
makeSignal (i, vec) = (i, makeSig vec)

makeSig :: V.Vector Double -> STFT.Signal
makeSig vec = STFT.MkSignal $ fmap (\x -> x C.:+ 0) vec

data Note = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab  deriving (Eq, Ord, Show, Enum)

fromChar :: Char -> Note
fromChar c = undefined

instance ToJSON Note where
    toJSON note = object ["note" .= (show note)]

instance PersistField Note where
  toPersistValue n = PersistText (T.pack $ show n)
  fromPersistValue (PersistText b) = undefined
  fromPersistValue x = Left $ "File.hs: When trying to deserialize a Note: expected PersistText, received: " <> (T.pack $ show x)

instance PersistFieldSql Note where
  sqlType _ = SqlString

data NoteCounter = MkCounter {
    counter :: Int,
    note    :: Note
}

twelthRoot :: Double
twelthRoot = 2 ** (1 / 12)

intToFreq :: Int -> TWM.Freq
intToFreq n = TWM.MkFreq $ 440 * twelthRoot ** fromIntegral n

intToNote :: Int -> Note
intToNote n = toEnum (abs $ n `mod` 12)

initCounter :: Note -> NoteCounter
initCounter n = MkCounter {
    counter = 0,
    note = n
}

closestFreq :: TWM.Freq -> [TWM.Freq] -> TWM.Freq
closestFreq f = snd . minimum . map (abs . subtract f &&& id)

type NoteMap = Map.Map TWM.Freq NoteCounter

-- |Look up table from frequency to note
-- Based on https://pages.mtu.edu/~suits/NoteFreqCalcs.html
freqToNoteTable :: NoteMap
freqToNoteTable = Map.fromList $ fmap (\i -> (intToFreq i, initCounter $ intToNote i)) [(-36)..24]

addToCounter :: NoteCounter -> NoteCounter
addToCounter noteCounter = MkCounter {
    counter = counter noteCounter + 1,
    note = note noteCounter
}

instance Eq NoteCounter where
    (MkCounter counter1 _) == (MkCounter counter2 _) = counter1 == counter2

instance Ord NoteCounter where
    (MkCounter counter1 _) `compare` (MkCounter counter2 _) = counter1 `compare` counter2

findf0 :: [TWM.Freq] -> Note
findf0 fs = note $ maximum $ Map.elems reduced
    where
        freqMap = flip closestFreq (Map.keys freqToNoteTable)
        hz = fmap freqMap (filter (/= 0) fs)
        adjust' = flip $ Map.adjust addToCounter
        reduced = foldl' adjust' freqToNoteTable hz







