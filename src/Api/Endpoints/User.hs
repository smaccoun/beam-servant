module Api.Endpoints.User where

import           App
import           AppPrelude
import           Control.Lens         hiding (element)
import qualified Crypto.Scrypt        as S
import           Data.Text.Encoding   (encodeUtf8)
import           Database.Beam
import qualified Database.Schema      as Schema
import           Database.Tables.User
import           Database.Transaction
import           Models.Credentials   (Email (..), Password (..))
import           Models.User
import           Servant

type UserAPI =
    "users"
        :> (
            Get '[JSON] [UserResponse]
        )

userAPI :: Proxy User
userAPI = Proxy

userServer :: UserResponse -> ServerT UserAPI AppM
userServer _ = getUsers

userTable :: DatabaseEntity be Schema.MyAppDb (TableEntity UserT)
userTable = Schema.appDb ^. Schema.users

getUsers :: AppM [UserResponse]
getUsers = do
  usersDB <- runQueryM $ select (all_ userTable)
  return $ map userApiFromUserDB usersDB

getUserByEmail :: Email -> AppM User
getUserByEmail (Email email') = do
  Config{..} <- ask
  userResult <- liftIO $ runQuerySingle getPool $
    select $
    do  users <- all_ (userTable)
        guard_ (users ^. userEmail ==. val_ email')
        pure users
  return $ userResult

createUser :: PGPool -> Email -> Password -> IO ()
createUser conn (Email email') (Password unencryptedPassword) = do
  encryptedPassword <- liftIO $ S.encryptPassIO S.defaultParams (S.Pass $ encodeUtf8 unencryptedPassword)
  runSql conn $ runInsert (insertStmt encryptedPassword)

  where
    insertStmt encryptedPassword = insert userTable $
        insertExpressions [ User default_ (val_ email') (val_ encryptedPassword)]
