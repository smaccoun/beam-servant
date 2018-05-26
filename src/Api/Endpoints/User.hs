{-# LANGUAGE TypeFamilies         #-}

module Api.Endpoints.User where

import           Api.Resource
import           App
import           AppPrelude
import           Control.Lens           hiding (element)
import qualified Crypto.Scrypt          as S
import           Data.Text.Encoding     (encodeUtf8)
import           Data.UUID              (UUID)
import           Database.Beam
import           Database.Crud
import           Database.MasterEntity  (table)
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
  encryptedPassword <- liftIO $ S.encryptPassIO S.defaultParams (S.Pass $ encodeUtf8 unencryptedPassword)
  createEntity userTable (User email' encryptedPassword)
