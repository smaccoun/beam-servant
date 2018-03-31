module Api.User where

import App
import AppPrelude
import Servant
import DB.Transaction
import Models.User
import Database.Beam
import Database.Beam.Postgres
import DBSchema

type UserAPI =
    "users"
        :> (
            Get '[JSON] [User]
        )

userAPI :: Proxy User
userAPI = Proxy

userServer :: ServerT UserAPI AppM
userServer = getUsers

getUsers :: AppM [User]
getUsers = do
  users <- runQuery $ runSelectReturningList $ select (all_ (_users appDb))
  return users





