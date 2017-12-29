{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Monad
import Control.Monad.IO.Class
import           Network.Wai.Handler.Warp as Warp

import Servant
import Servant.Multipart

import qualified Data.ByteString.Lazy as LBS

-- Our API, which consists in a single POST endpoint at /
-- that takes a multipart/form-data request body and
-- pretty-prints the data it got to stdout before returning 0.
--type UploadAPI = "upload" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer

newtype Song = Song { wavFile :: Text } deriving (Eq, Ord, Show)

instance FromMultipart Mem Song where
  fromMultipart multipartData =
      Song <$> lookupInput "recording" multipartData

type UploadAPI = "upload" :> MultipartForm Mem Song :> Post '[JSON] Integer

type AssetsAPI = Raw

type API = UploadAPI :<|> AssetsAPI

api :: Proxy API
api = Proxy

server :: Server API
server = upload :<|> static

-- The handler for our single endpoint.
-- Its concrete type is:
--   MultipartData -> Handler Integer
--
-- MultipartData consists in textual inputs,
-- accessible through its "inputs" field, as well
-- as files, accessible through its "files" field.
{-
upload :: Server UploadAPI
upload multipartData = do
  liftIO $ do
    putStrLn "Inputs:"
    forM_ (inputs multipartData) $ \input ->
      putStrLn $ "  " ++ show (iName input)
            ++ " -> " ++ show (iValue input)

    forM_ (files multipartData) $ \file -> do
      let content = fdPayload file
      putStrLn $ "Content of " ++ show (fdFileName file)
      LBS.putStr content

  return 0
-}

upload :: Server UploadAPI
upload multipartData = do
  liftIO $ do
    putStrLn "Inputs:"
    forM_ (inputs multipartData) $ \input ->
      putStrLn $ "  " ++ show (iName input)
            ++ " -> " ++ show (iValue input)

    forM_ (files multipartData) $ \file -> do
      let content = fdPayload file
      putStrLn $ "Content of " ++ show (fdFileName file)
      LBS.putStr content

  return 0


static :: Server Raw
static = serveDirectoryFileServer "assets"

startServer :: IO ()
startServer = Warp.run 8080 (serve api server)

run :: IO ()
run = startServer
  -- we fork the server in a separate thread and send a test
  -- request to it from the main thread.
