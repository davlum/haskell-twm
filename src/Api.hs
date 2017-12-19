{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Data.Proxy
import           Data.Text

import           Database.Persist

import           Models

import           Servant.API



type Api =
       "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
  :<|> "user" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe User)
  :<|> "record" :> Raw

api :: Proxy Api
api = Proxy
