{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Models.User where

import           AppPrelude
import           Data.Aeson
import           Database.Beam
import           Database.Beam.Postgres
import           GHC.Generics           (Generic)

type User = UserT Identity

data UserT f
    = User
    { _userEmail    :: Columnar f Text
    , _userPassword :: Columnar f Text }
    deriving (Generic)

instance Beamable UserT
instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
  primaryKey = UserId . _userEmail

instance Beamable (PrimaryKey UserT)
deriving instance ToJSON User

data MyAppDb f =
  MyAppDb
    { _users :: f (TableEntity UserT)
    } deriving Generic

instance Database be MyAppDb

appDb :: DatabaseSettings be MyAppDb
appDb = defaultDbSettings

