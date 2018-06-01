module Auth where

import           AppPrelude
import           Crypto.JOSE.JWK           (JWK)
import           Network.Wai.Test          (WaiTestFailure (..))
import           Servant.Auth.Server       (defaultJWTSettings)
import           Data.CaseInsensitive      (mk)
import           Api.Endpoints.Login
import           Config.AppConfig          (HasDBConn)
import           Models.Credentials
import           Models.Login
import           Network.HTTP.Types.Header (Header)

validTestUserLogin :: Login
validTestUserLogin = Login (Email "test@blah.com") (Password "testPassword")

mkAuthHeader :: Text -> [Header] -> [Header]
mkAuthHeader jwt curHeaders =
  curHeaders ++ [(mk "Authorization", encodeUtf8 $ "Bearer " <> jwt)]

loginTestUser
  :: (MonadReader r m, MonadIO m, HasDBConn r)
  => JWK
  -> m (Either WaiTestFailure Text)
loginTestUser jwk = do
  eitherLoginResponse <- loginUserPassword (defaultJWTSettings jwk)
                                           validTestUserLogin
  case eitherLoginResponse of
    Right lr -> return $ Right $ jwtToken lr
    Left  _  -> return $ Left $ WaiTestFailure "failed to authorize"
