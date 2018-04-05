module Api.Login where

import           Api.User             (getUserByEmail)
import           App
import           AppPrelude
import qualified Crypto.Scrypt        as S
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Text            (Text)
import           Data.Text.Encoding   (encodeUtf8)
import           Models.User          (User)
import           Models.User
import           Servant
import           Servant.Auth.Server

data Login = Login { email :: Text , password :: Text }
   deriving (Eq, Show, Read, Generic)

instance FromJSON Login
instance ToJWT User
instance FromJWT User

type LoginAPI = "login"
       :> ReqBody '[JSON] Login :> Post '[JSON] Text

loginAPI :: Proxy LoginAPI
loginAPI = Proxy

loginServer :: JWTSettings -> Login -> AppM Text
loginServer jwtCfg (Login email loginPassword) = do
  user <- getUserByEmail email
  if hasCorrectPassword user loginPassword then do
    print $ (show user :: Text)
    eitherJWT <- liftIO $ makeJWT user jwtCfg Nothing
    case eitherJWT of
      Left e    -> panic $ show e
      Right jwt -> return $ decodeUtf8 $ BSL.toStrict jwt
  else
     return "Bad Password"

hasCorrectPassword :: User -> Text -> Bool
hasCorrectPassword (User _ _ _userPassword) password =
  fst $  S.verifyPass S.defaultParams (S.Pass $ encodeUtf8 password) encrypted
  where
    encrypted = (S.EncryptedPass $ encodeUtf8 _userPassword)
