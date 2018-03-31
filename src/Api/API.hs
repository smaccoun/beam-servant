module Api.API where

import App
import AppPrelude
import Api.User
import Models.User
import Servant
import Data.Text (Text)
import Data.Aeson
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BSL
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

---------------------------------------------------------------

instance ToJWT User
instance FromJWT User

data Login = Login { username :: Text , password :: Text }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login
instance ToJWT Login
instance FromJWT Login

type Protected
   = "email" :> Get '[JSON] Text

protected :: AuthResult User -> ServerT Protected AppM
protected (Authenticated user) = return (_userEmail user)
protected Servant.Auth.Server.BadPassword = do
  print "Bad Password"
  throwAll err401
protected Servant.Auth.Server.NoSuchUser = do
  print "NO SUCH USER"
  throwAll err401
protected Indefinite = do
  print "Indefinite"
  throwAll err401


type Unprotected =
       Get '[JSON] Text
  :<|> "login"
       :> ReqBody '[JSON] Login :> Post '[JSON] Text

unprotectedProxy :: Proxy Unprotected
unprotectedProxy = Proxy

unprotected :: JWTSettings -> ServerT Unprotected AppM
unprotected jwts =
       return "hello world"
  :<|> login jwts

login :: JWTSettings -> Login -> AppM Text
login jwts login = do
  eitherJWT <- liftIO $ makeJWT login jwts Nothing
  case eitherJWT of
    Left e -> panic $ show e
    Right jwt -> return $ decodeUtf8 $ BSL.toStrict jwt

type API auths = (Auth auths User :> Protected) :<|> Unprotected

api :: Proxy (API '[JWT])
api = Proxy


server :: JWTSettings -> ServerT (API auths) AppM
server jwts =
       protected
  :<|> unprotected jwts
