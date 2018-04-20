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
import           Lib.TWM

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
        Nothing -> error "File was not properly not recieved"
        Just file -> liftIO $ do
          audio <- readWav file
          let result = twmWrapper audio
          print result
          return 0


static :: Server Raw
static = serveDirectoryFileServer "assets"

startServer :: IO ()
startServer = Warp.run 3000 (serve api server)

run :: IO ()
run = startServer
  -- we fork the server in a separate thread and send a test
  -- request to it from the main thread.
