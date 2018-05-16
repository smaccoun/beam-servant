{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.MasterEntity where

import           AppPrelude
import           Control.Lens    hiding (element, (.=))
import           Data.Time.Clock (UTCTime)
import           Data.UUID       (UUID)
import           Database.Beam
import           GHC.Generics    (Generic)

data AppEntity table f
    = AppEntity
    { _appId      :: C f UUID
    , _table      :: table f
    , _created_at :: C f UTCTime
    , _updated_at :: C f UTCTime
    } deriving (Generic)

instance (Beamable table) => Beamable (AppEntity table)

makeLenses ''AppEntity

instance (Beamable table, Typeable table) => Table (AppEntity table) where
    data PrimaryKey (AppEntity table) f = MyAppKey (Columnar f UUID) deriving (Generic, Beamable)
    primaryKey = MyAppKey <$> _appId
