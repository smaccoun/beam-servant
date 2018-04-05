module Api.Login where

import           Api.User             (getUserByEmail)
import           App
import           AppPrelude
import           Control.Lens
import qualified Crypto.Scrypt        as S
import qualified Data.ByteString.Lazy as BSL
import           Data.Text            (Text)
import           Data.Text.Encoding   (encodeUtf8)
import           Database.Tables.User (User, userPassword)
import           Models.Login
import           Servant
import           Servant.Auth.Server

type LoginAPI = "login"
       :> ReqBody '[JSON] Login :> Post '[JSON] Text

loginAPI :: Proxy LoginAPI
loginAPI = Proxy

loginServer :: JWTSettings -> Login -> AppM Text
loginServer jwtCfg (Login email loginPassword) = do
  user <- getUserByEmail email
  if hasCorrectPassword user loginPassword then do
    print (show user :: Text)
    eitherJWT <- liftIO $ makeJWT user jwtCfg Nothing
    case eitherJWT of
      Left e    -> panic $ show e
      Right jwt -> return $ decodeUtf8 $ BSL.toStrict jwt
  else
     return "Bad Password"

hasCorrectPassword :: User -> Text -> Bool
hasCorrectPassword user password =
  fst $  S.verifyPass S.defaultParams (S.Pass $ encodeUtf8 password) encrypted
  where
    encrypted = S.EncryptedPass $ encodeUtf8 $ user ^. userPassword
