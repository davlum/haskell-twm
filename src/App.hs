{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module App where

import           Control.Monad.IO.Class
import           Network.Wai.Handler.Warp as Warp
import           Servant
import           Servant.Multipart

-- Probably redundant
import           Control.Monad
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text
import           TWM

-- Our API, which consists in a single POST endpoint at /
-- that takes a multipart/form-data request body and
-- pretty-prints the data it got to stdout before returning 0.
--type UploadAPI = "upload" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer

type UploadAPI = "upload" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] Integer

type AssetsAPI = Raw

type API = UploadAPI :<|> AssetsAPI


api :: Proxy API
api = Proxy

server :: Server API
server = uploadS :<|> static

uploadS :: Server UploadAPI
uploadS multipartData =
  let maybeFile = fmap fdPayload (lookupFile "recording" multipartData)
  in case maybeFile of
        Nothing -> error "File was not able to be saved"
        Just file -> liftIO $ do
          audio <- readWav file
          print $ fst audio
          return 0


static :: Server Raw
static = serveDirectoryFileServer "assets"

startServer :: IO ()
startServer = Warp.run 8080 (serve api server)

run :: IO ()
run = startServer
  -- we fork the server in a separate thread and send a test
  -- request to it from the main thread.
