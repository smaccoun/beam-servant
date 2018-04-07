{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Models.Credentials where

import           AppPrelude
import           Control.Lens (makeLenses)
import           Data.Aeson
import           Data.Text    (Text)

newtype Email =
  Email {_unEmail :: Text}
    deriving (Generic, Eq, Show, Read)

instance FromJSON Email where
  parseJSON (String v) = pure (Email v)
  parseJSON _          = mzero

instance ToJSON Email where
  toJSON (Email email') = object ["email" .= email']

makeLenses ''Email

newtype Password =
  Password {unPassword :: Text}
    deriving (Generic, Eq, Show, Read)

instance FromJSON Password where
  parseJSON (String v) = pure (Password v)
  parseJSON _          = mzero
