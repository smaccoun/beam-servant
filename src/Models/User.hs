module Models.User where

import AppPrelude
import Data.Aeson
import Data.Text
import           GHC.Generics                 (Generic)

data User =
  User
   { firstName :: Text
   , lastName :: Text
   } deriving (Generic)

instance ToJSON User

