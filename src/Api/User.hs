module Api.User where

import App
import AppPrelude
import Servant
import DB.Transaction
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


usersQ =
  runQuery query
  where
    query = runSelectReturningList $ select (all_ (_users appDb))

getUsers :: AppM [User]
getUsers = do
  users <- usersQ
  return users




