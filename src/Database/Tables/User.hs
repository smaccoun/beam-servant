{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Tables.User where

import           AppPrelude
import           Control.Lens                         hiding (element, (.=))
import qualified Crypto.Scrypt                        as S
import           Data.Aeson
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Postgres
import           Database.MasterEntity
import           Database.PostgreSQL.Simple.FromField
import           GHC.Generics                         (Generic)
import           Models.Credentials                   (Email (..))
import           Prelude                              (String)

type UserT = AppEntity UserBase

data UserBase f =
  User
   {_email    :: C f Text
   ,_password :: C f S.EncryptedPass
   } deriving (Generic)

instance Beamable UserBase
type User = UserBase Identity
type UserEntity = UserT Identity

makeLenses ''UserBase

deriving instance Generic S.EncryptedPass

deriving instance Show User
instance FromField S.EncryptedPass where
  fromField field mb_bytestring = S.EncryptedPass <$> fromField field mb_bytestring

instance FromField Email where
  fromField field mb_bytestring = Email <$> fromField field mb_bytestring
deriving instance FromBackendRow Postgres S.EncryptedPass
deriving instance FromBackendRow Postgres Email

instance HasSqlValueSyntax be Prelude.String => HasSqlValueSyntax be Email where
  sqlValueSyntax (Email e) = autoSqlValueSyntax e

instance HasSqlValueSyntax be Prelude.String => HasSqlValueSyntax be S.EncryptedPass where
  sqlValueSyntax = autoSqlValueSyntax

instance ToJSON User where
  toJSON User{..} = object
    ["email" .= _email ]

instance ToJSON (AppEntity UserBase Identity)
