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
import           Database.MasterEntity  (AppEntity (..), table)
import           Database.Crud
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
  getEntities userTable

getUser :: UUID -> AppM UserEntity
getUser userId' = do
  getEntity userTable userId'

getUserByEmail :: Email -> AppM UserEntity
getUserByEmail (Email email') = do
  userResult <- runQuerySingle $
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
              (userInsert email' encryptedPassword)
              default_
              (val_ now)
          ]

userInsert :: (SqlValable (Columnar f S.EncryptedPass),
                SqlValable (Columnar f Text)) =>
                 HaskellLiteralForQExpr (Columnar f Text)
              -> HaskellLiteralForQExpr (Columnar f S.EncryptedPass)
              -> UserBase f
userInsert email' encryptedPassword =
  (User (val_ email') (val_ encryptedPassword))
