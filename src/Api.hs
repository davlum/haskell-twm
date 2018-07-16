{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Api where

import qualified Lib.Util          as U
import           Servant
import           Servant.Multipart

type UploadAPI = "upload" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] U.Note

type AssetsAPI = Raw

type API = UploadAPI :<|> AssetsAPI

api :: Proxy API
api = Proxy
