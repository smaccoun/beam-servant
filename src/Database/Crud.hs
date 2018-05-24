{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Crud where

import           App
import           AppPrelude
import           Control.Lens                hiding (element)
import           Data.Optional
import           Data.UUID                   (UUID)
import           Database.Beam
import           Database.Beam.Backend.Types
import           Database.Beam.Postgres
import           Database.MasterEntity
import           Database.Schema
import           Database.Transaction
import           GHC.Generics                (Generic)

getEntities :: ( Beamable inner
               , Typeable inner
               , Generic (inner Identity)
               , Generic (inner Exposed)
               , Database.Beam.Backend.Types.GFromBackendRow
                          Postgres (Rep (inner Exposed)) (Rep (inner Identity))
               , MonadReader Config m
               , MonadIO m
               )
            => DatabaseEntity Postgres MyAppDb (TableEntity (AppEntity inner))
            -> m [AppEntity inner Identity]
getEntities t = do
  runQueryM Default
    $ orderBy_ (\e -> (desc_ (e ^. updated_at)))
    $ all_ t


getEntity :: ( Beamable inner
               , Typeable inner
               , Generic (inner Identity)
               , Generic (inner Exposed)
               , GFromBackendRow Postgres (Rep (inner Exposed)) (Rep (inner Identity))
               , MonadReader Config m
               , MonadIO m
               )
            => DatabaseEntity Postgres MyAppDb (TableEntity (AppEntity inner))
            -> UUID
            -> m (AppEntity inner Identity)
getEntity t uuid' = do
  result <- runQuerySingle $
    do allItems <- (all_ t)
       guard_ (allItems ^. appId ==. val_ uuid')
       pure allItems
  return result
