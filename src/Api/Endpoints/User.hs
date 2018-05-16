module Api.Endpoints.User where

import           Api.Resource
import           App
import           AppPrelude
import           Control.Lens           hiding (element)
import qualified Crypto.Scrypt          as S
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Data.UUID              (UUID)
import           Database.Beam
import           Database.Beam.Postgres (PgInsertSyntax)
import           Database.MasterEntity  (AppEntity (..), appId, table)
import           Database.Schema        (userTable)
import           Database.Tables.User
import           Database.Transaction
import           Models.Credentials     (Email (..), Password (..))
import           Models.User            (UserResponse)
import           Servant

type UserAPI = RResourceAPI "users" UserEntity UUID

userServer :: UserResponse -> ServerT UserAPI AppM
userServer _ = rResourceServer getUsers getUser

getUsers :: AppM [UserEntity]
getUsers = do
  usersDB <- runQueryM $ select (all_ userTable)
  return $ usersDB

getUser :: UUID -> AppM UserEntity
getUser userId' = do
  userResult <- runQuerySingle $ select $
    do users <- (all_ userTable)
       guard_ (users ^. appId ==. val_ userId')
       pure users
  return $ userResult

getUserByEmail :: Email -> AppM UserEntity
getUserByEmail (Email email') = do
  Config{..} <- ask
  userResult <- runQuerySingle $
    select $
    do  users <- all_ (userTable)
        guard_ (users ^. table ^. email ==. val_ email')
        pure users
  return $ userResult

createUser :: (MonadIO m, MonadReader Config m) => Email -> Password -> m ()
createUser (Email email') (Password unencryptedPassword) = do
  now <- liftIO $ getCurrentTime
  encryptedPassword <- liftIO $ S.encryptPassIO S.defaultParams (S.Pass $ encodeUtf8 unencryptedPassword)
  _ <- runInsertM (insertStmt encryptedPassword now)
  return ()

  where
    insertStmt :: S.EncryptedPass -> UTCTime -> SqlInsert PgInsertSyntax
    insertStmt encryptedPassword now = insert userTable $
        insertExpressions
          [ AppEntity
              default_
              (User (val_ email') (val_ encryptedPassword))
              default_
              (val_ now)
          ]
