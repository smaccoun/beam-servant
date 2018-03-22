{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( startApp
    ) where

import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.API
import Data.Aeson

import AppPrelude
import qualified Data.Text as T

import Api.User


type API =
         Get '[JSON] T.Text
    :<|> UserAPI

api :: Proxy API
api = Proxy

server :: Server API
server = return "Hello World"
    :<|> userServer


startApp :: IO ()
startApp = do
    Warp.run 8080 app

app :: Application
app =
    serve api server

