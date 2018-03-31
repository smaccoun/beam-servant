module Api.API where

import App
import AppPrelude
import Api.User
import Servant
import Data.Text (Text)

type API =
       Get '[JSON] Text
  :<|> UserAPI

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server =
       return "hello world"
  :<|> userServer
