module Api.API where

import App
import AppPrelude
import Api.User
import Servant
import Data.Text (Text)

type Unprotected =
       Get '[JSON] Text
  :<|> UserAPI

unprotectedProxy :: Proxy Unprotected
unprotectedProxy = Proxy

unprotectedS :: ServerT Unprotected AppM
unprotectedS =
       return "hello world"
  :<|> userServer


type API = Unprotected

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = unprotectedS
