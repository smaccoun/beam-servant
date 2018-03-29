{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, TypeFamilies, DeriveGeneric, OverloadedStrings, UndecidableInstances, DeriveAnyClass #-}

module Models.User where

import AppPrelude
import Data.Aeson
import Data.Text
import           GHC.Generics                 (Generic)
import Database.Beam

data UserT f
    = UserT
    { _userEmail     :: Columnar f Text
    , _userPassword  :: Columnar f Text }
    deriving (Generic)

data User =
  User
    { email :: Text
    , password :: Text
    } deriving (Generic, ToJSON)



