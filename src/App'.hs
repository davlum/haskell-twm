{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App' where

import           Control.Monad.Logger     (runStderrLoggingT)

import           Data.String.Conversions
import           Data.Proxy

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Text.Encoding (encodeUtf8)
import Network (withSocketsDo)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
import Servant.Multipart
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import           Debug.Trace
import           Servant
import qualified Data.ByteString.Lazy as LBS
import           Data.Text

import           Api
import           Models

server :: ConnectionPool -> Server API
server pool =
  userAddH :<|>
  userGetH :<|>
  static

  where
    userAddH newUser = liftIO $ userAdd newUser
    userGetH name    = liftIO $ userGet name

    userAdd :: User -> IO (Maybe (Key User))
    userAdd newUser = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [UserName ==. userName newUser] []
      case exists of
        Nothing -> Just <$> insert newUser
        Just _  -> return Nothing

    userGet :: Text -> IO (Maybe User)
    userGet name = flip runSqlPersistMPool pool $ do
      mUser <- selectFirst [UserName ==. name] []
      return $ entityVal <$> mUser

    static :: Server Raw
    static = serveDirectoryFileServer "assets"

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return $ app pool

run :: FilePath -> IO ()
run sqliteFile = do
  let port = 3000
  putStrLn $ "listening on port " ++ show port
  Warp.run port =<< mkApp sqliteFile
