module Models.Login where

import           AppPrelude
import           Data.Aeson
import           Data.Text            (Text)
import           Database.Tables.User (User)
import           Servant.Auth.Server

data Login = Login { email :: Text , password :: Text }
   deriving (Eq, Show, Read, Generic)


instance FromJSON Login
instance ToJWT User
instance FromJWT User
