module Api.Login where

import App
import AppPrelude
import Data.Text (Text)
import Data.Aeson
import Data.Text.Encoding (decodeUtf8)
import Servant.Auth.Server
import Servant
import qualified Data.ByteString.Lazy as BSL

data Login = Login { username :: Text , password :: Text }
   deriving (Eq, Show, Read, Generic)


instance ToJSON Login
instance FromJSON Login
instance ToJWT Login
instance FromJWT Login

type LoginAPI = "login"
       :> ReqBody '[JSON] Login :> Post '[JSON] Text

loginAPI :: Proxy LoginAPI
loginAPI = Proxy

loginServer :: JWTSettings -> Login -> AppM Text
loginServer jwtCfg login = do
  eitherJWT <- liftIO $ makeJWT login jwtCfg Nothing
  case eitherJWT of
    Left e -> panic $ show e
    Right jwt -> return $ decodeUtf8 $ BSL.toStrict jwt
