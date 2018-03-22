module Api.User where

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

userServer :: Server UserAPI
userServer = return (User "Bob" "Smith")


