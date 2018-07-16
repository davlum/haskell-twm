{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module App where

import qualified Api                      as Api
import           Control.Monad.IO.Class
import qualified Lib.TWM                  as TWM
import qualified Lib.Util                 as U
import           Network.Wai.Handler.Warp as Warp
import           Servant
import           Servant.Multipart

server :: Server Api.API
server = uploadS :<|> static

uploadS :: Server Api.UploadAPI
uploadS multipartData =
  let maybeFile = fmap fdPayload (lookupFile "recording" multipartData)
  in case maybeFile of
        Nothing -> error "File was not properly not recieved"
        Just file -> liftIO $ do
          audio <- U.readWav file
          let hertz = TWM.twmWrapper $ U.makeSignal audio
          return $ U.findf0 hertz


static :: Server Raw
static = serveDirectoryFileServer "static"

startServer :: Int -> IO ()
startServer port = Warp.run port (serve Api.api server)

run :: IO ()
run = do
  let port = 8080
  print $ "Server started at port " ++ show port
  startServer port
