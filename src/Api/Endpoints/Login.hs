{-# LANGUAGE DeriveAnyClass #-}

module Api.Endpoints.Login where

import           Api.Endpoints.User    (getUserByEmail)
import           AppPrelude
import           Config.AppConfig
import           Control.Lens
import qualified Crypto.Scrypt         as S
import           Data.Aeson
import qualified Data.ByteString.Lazy  as BSL
import           Data.Text             (Text)
import           Data.Text.Encoding    (encodeUtf8)
import           Data.UUID             (UUID)
import           Database.MasterEntity (appId, baseTable)
import           Database.Tables.User  as UT (UserEntity, password)
import           Models.Credentials    (Password (..))
import           Models.Login
import           Models.User           (userApiFromUserDB)
import           Servant
import           Servant.Auth.Server

data LoginResponse =
  LoginResponse
    {jwtToken :: Text
    ,userId   :: UUID
    } deriving (Generic, ToJSON)


type LoginAPI = "login"
       :> ReqBody '[JSON] Login :> Post '[JSON] LoginResponse

loginAPI :: Proxy LoginAPI
loginAPI = Proxy


loginServer :: JWTSettings -> ServerT LoginAPI AppM
loginServer jwtSettings = handleUserPasswordLogin jwtSettings

handleUserPasswordLogin :: JWTSettings -> Login -> AppM LoginResponse
handleUserPasswordLogin jwtSettings login' = do
  loginResult <- loginUserPassword jwtSettings login'
  case loginResult of
    Right loginResponse -> return loginResponse
    Left e' -> throwError e'

loginUserPassword :: (MonadIO m, MonadReader r m, HasDBConn r)
    => JWTSettings
    -> Login
    -> m (Either ServantErr LoginResponse)
loginUserPassword jwtCfg (Login loginEmail loginPassword) = do
  userResult <- getUserByEmail loginEmail
  if hasCorrectPassword userResult loginPassword
    then do
      loginResponse <- (getLoginResponse userResult jwtCfg)
      return $ Right loginResponse
    else return $ Left $ err500 { errBody = "Incorrect Password" }

getLoginResponse :: MonadIO m =>
             UserEntity
          -> JWTSettings
          -> m LoginResponse
getLoginResponse userResult jwtCfg = do
      let userApi = userApiFromUserDB userResult
      eitherJWT <- liftIO $ makeJWT userApi jwtCfg Nothing
      case eitherJWT of
        Left  e   -> panic $ show e
        Right jwt -> return $ LoginResponse
          { jwtToken = decodeUtf8 $ BSL.toStrict jwt
          , userId   = userResult ^. appId
          }

hasCorrectPassword :: UserEntity -> Password -> Bool
hasCorrectPassword userT (Password password') = fst $ S.verifyPass
  S.defaultParams
  (S.Pass $ encodeUtf8 password')
  (userT ^. baseTable ^. UT.password)
