module Api.Endpoints.User where

import           Api.Resource
import           App
import           AppPrelude
import           Control.Lens         hiding (element)
import qualified Crypto.Scrypt        as S
import           Data.Text.Encoding   (encodeUtf8)
import           Data.Time.Clock      (getCurrentTime)
import           Database.Beam
import           Database.Schema      (userTable)
import           Database.Tables.User
import           Database.Transaction
import           Models.Credentials   (Email (..), Password (..))
import           Models.User          (UserResponse)
import           Servant

type UserAPI = RResourceAPI "users" UserEntity UserID

userServer :: UserResponse -> ServerT UserAPI AppM
userServer _ = rResourceServer getUsers getUser

getUsers :: AppM [UserEntity]
getUsers = do
  usersDB <- runQueryM $ select (all_ userTable)
  return $ usersDB

getUser :: UserID -> AppM UserEntity
getUser userId' = do
  userResult <- runQuerySingle $ select $
    do users <- (all_ userTable)
       guard_ (users ^. userId ==. val_ userId')
       pure users
  return $ userResult

getUserByEmail :: Email -> AppM UserEntity
getUserByEmail (Email email') = do
  Config{..} <- ask
  userResult <- runQuerySingle $
    select $
    do  users <- all_ (userTable)
        guard_ (users ^. user ^. email ==. val_ email')
        pure users
  return $ userResult

createUser :: PGPool -> Email -> Password -> IO ()
createUser conn (Email email') (Password unencryptedPassword) = do
  now <- getCurrentTime
  encryptedPassword <- liftIO $ S.encryptPassIO S.defaultParams (S.Pass $ encodeUtf8 unencryptedPassword)
  runSql conn $ runInsert (insertStmt encryptedPassword now)

  where
    insertStmt encryptedPassword now = insert userTable $
        insertExpressions
          [ UserT
              default_
              (User (val_ email') (val_ encryptedPassword))
              default_
              (val_ now)
          ]
