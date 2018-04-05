module Api.API where

import           Api.Login
import           Api.User
import           App
import           AppPrelude
import           Data.Swagger
import           Data.Text                           (Text)
import           Database.Tables.User
import           Models.Login
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import           Servant.Swagger

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

type API auths =
       (Auth auths User :> Protected)
  :<|> Unprotected

api :: Proxy (API '[JWT])
api = Proxy


serverAPI :: JWTSettings -> ServerT (API auths) AppM
serverAPI jwts =
       protected
  :<|> unprotected jwts


-- SWAGGER

swaggerUnprotected :: Swagger
swaggerUnprotected = toSwagger unprotectedProxy

instance ToSchema Login
