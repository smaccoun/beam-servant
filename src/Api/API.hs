module Api.API where

import App
import AppPrelude
import Api.User
import Api.Login
import Models.User
import Servant
import Data.Text (Text)
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

---------------------------------------------------------------
type Protected
   = "email" :> Get '[JSON] Text
   :<|> UserAPI

   

protected :: AuthResult User -> ServerT Protected AppM
protected (Authenticated (User _ _userEmail _)) =
       return _userEmail
  :<|> userServer

protected _ = throwAll err401

type Unprotected =
       Get '[JSON] Text
  :<|> LoginAPI

unprotectedProxy :: Proxy Unprotected
unprotectedProxy = Proxy

unprotected :: JWTSettings -> ServerT Unprotected AppM
unprotected jwts =
       return "hello world"
  :<|> loginServer jwts

type API auths = (Auth auths User :> Protected) :<|> Unprotected

api :: Proxy (API '[JWT])
api = Proxy


server :: JWTSettings -> ServerT (API auths) AppM
server jwts =
       protected
  :<|> unprotected jwts



  
