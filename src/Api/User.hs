module Api.User where

import App
import AppPrelude
import Servant
import Data.Text
import Data.Aeson
import Models.User


type UserAPI =
    "users"
        :> (
            Get '[JSON] User
        )

userAPI :: Proxy User
userAPI = Proxy

userServer :: ServerT UserAPI AppM
userServer = return (User "Bob" "Smith")


