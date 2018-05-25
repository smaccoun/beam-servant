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
import           Database.Crud
import           Database.MasterEntity  (AppEntity (..), table)
import           Database.Schema        (userTable)
import           Database.Tables.User
import           Database.Transaction
import           Models.Credentials     (Email (..), Password (..))
import           Models.User            (UserResponse)
import           Pagination
import           Servant

type UserAPI = RResourceAPI "users" PaginatedResult UserEntity UUID

userServer :: UserResponse -> ServerT UserAPI AppM
userServer _ = do
  rResourceServer getUsers getUser

getUsers :: (MonadIO m, MonadReader r m, HasDBConn r) =>
         Maybe Limit ->
         Maybe Offset ->
         Maybe Text ->
          m (PaginatedResult UserEntity)
getUsers mbLimit mbPage _ =
  getEntities mbLimit mbPage DefaultOrder userTable

getUser :: (MonadIO m, MonadReader r m, HasDBConn r) => UUID -> m UserEntity
getUser userId' =
  getEntity userTable userId'

getUserByEmail :: (MonadIO m, MonadReader r m, HasDBConn r) => Email -> m UserEntity
getUserByEmail (Email email') = do
  userResult <- runQuerySingle $ select $
    do  users <- all_ (userTable)
        guard_ (users ^. table ^. email ==. val_ email')
        pure users
  return $ userResult

createUser :: (MonadIO m, MonadReader r m, HasDBConn r) => Email -> Password -> m ()
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
              (userInsert email' encryptedPassword)
              default_
              (val_ now)
          ]

userInsert :: (SqlValable (Columnar f S.EncryptedPass),
                SqlValable (Columnar f Text))
              => HaskellLiteralForQExpr (Columnar f Text)
              -> HaskellLiteralForQExpr (Columnar f S.EncryptedPass)
              -> UserBase f
userInsert email' encryptedPassword =
  (User (val_ email') (val_ encryptedPassword))
