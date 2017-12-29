{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api where

import           Servant
import           Data.Text
import           Database.Persist
import           Models
import           Servant.Multipart

newtype Song = Song { wavFile :: Text } deriving (Eq, Ord, Show)

instance FromMultipart Mem Song where
  fromMultipart multipartData =
      Song <$> lookupInput "recording" multipartData

type UploadAPI = "upload" :> MultipartForm Mem Song :> Post '[JSON] Integer

type SongAPI =
       "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
  :<|> "user" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe User)
  :<|> Raw

type API = UploadAPI :<|> SongAPI

api :: Proxy API
api = Proxy
