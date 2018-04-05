{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.User where

import           AppPrelude
import           Data.Aeson
import Data.UUID (UUID)
import           Database.Beam
import           GHC.Generics           (Generic)
import Control.Lens hiding (element)

type UserID = UUID

data UserT f
    = User
    { _userId       :: Columnar f UserID
    , _userEmail    :: Columnar f Text
    , _userPassword :: Columnar f Text
    } deriving (Generic)


type User = UserT Identity

makeLenses ''UserT

instance Beamable UserT
instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f UUID) deriving Generic
  primaryKey = UserId . _userId

instance Beamable (PrimaryKey UserT)
deriving instance Show User
deriving instance ToJSON User
deriving instance FromJSON User
