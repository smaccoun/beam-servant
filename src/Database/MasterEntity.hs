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
import           Control.Lens         hiding (element, (.=))
import           Data.Aeson           (ToJSON)
import qualified Data.Aeson           as A
import           Data.Time.Clock      (UTCTime)
import           Data.UUID            (UUID)
import           Database.Beam
import           GHC.Generics         (Generic)

data AppEntity table f
    = AppEntity
    { _appId      :: C f UUID
    , _baseTable  :: table f
    , _created_at :: C f UTCTime
    , _updated_at :: C f UTCTime
    } deriving (Generic)

instance (Beamable table) => Beamable (AppEntity table)

makeLenses ''AppEntity

instance (Beamable table, Typeable table) => Table (AppEntity table) where
    data PrimaryKey (AppEntity table) f = MyAppKey (Columnar f UUID) deriving (Generic, Beamable)
    primaryKey = MyAppKey <$> _appId

dropLensUnderOption :: A.Options
dropLensUnderOption = A.defaultOptions { A.fieldLabelModifier = drop 1 }

instance (ToJSON (t Identity)) => ToJSON (AppEntity t Identity) where
  toJSON appEntity = A.object $
        [("meta",  A.object $
            [("id" , appEntity ^. appId & A.toJSON)
            ,("createdAt" , appEntity ^. created_at & A.toJSON)
            ,("updatedAt" , appEntity ^. updated_at & A.toJSON)
            ]
          )
        ,("baseEntity", A.toJSON (appEntity ^. baseTable))
        ]

