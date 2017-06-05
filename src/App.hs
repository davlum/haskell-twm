{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Control.Monad.Trans.Except
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import TWM
import qualified Data.Vector as V

type SongApi =
 "song" :> Get '[JSON] [Song] :<|>
 "song" :> Capture "songId" Integer :> Get '[JSON] Song
 "song" :> "add" :> Capture "songPath" String


userApi :: Proxy UserApi
userApi = Proxy

run :: IO ()
run = do
 let port = 8000
     settings =
      setPort port $
       setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
       defaultSettings
 runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve userApi server

server :: Server UserApi
server =
  getSongs :<|>
  getSongById :<|>
  addSong

getSongs :: Handler [Song]
getSongs = return [

getSongById :: Integer -> Handler Song
getSongById = \case

addSong :: String -> Song
addSong str = Song $ 1 str (twmf0 $ readWav s)

exampleSong :: Song
exampleSong = Song 0 Nothing V.empty

data Song
 = Song {
   songId :: Integer
   songPath :: Maybe String
   songData :: V.Vector Double
        } deriving (Eq, Show, Generic)

instance ToJSON Song
instance FromJSON Song
