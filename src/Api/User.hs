module Api.User where

import App
import AppPrelude
import Servant
import Data.Text
import Data.Aeson
import Models.User
import Database.Beam

type UserAPI =
    "users"
        :> (
            Get '[JSON] [User]
        )

userAPI :: Proxy User
userAPI = Proxy

userServer :: ServerT UserAPI AppM
userServer = getUsers


usersQ conn = do
  withDatabase conn $ do
    users <- runSelectReturningList $ select (all_ (_users appDb))
    return users

getUsers :: AppM [User]
getUsers = do
  conn <- getConn
  users <- liftIO $ usersQ conn
  return users




