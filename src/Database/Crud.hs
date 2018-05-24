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
import           Data.UUID                   (UUID)
import           Database.Beam
import           Database.Beam.Backend.Types
import           Database.Beam.Postgres
import           Database.MasterEntity
import           Database.Schema
import           Database.Transaction
import           GHC.Generics                (Generic)
import           Pagination
import           Pagination                  (Pagination (..))

data OrderArg = DefaultOrder | CustomOrder Text

getEntities :: ( Beamable inner
               , Typeable inner
               , Generic (inner Identity)
               , Generic (inner Exposed)
               , Database.Beam.Backend.Types.GFromBackendRow
                          Postgres (Rep (inner Exposed)) (Rep (inner Identity))
               , MonadReader r m
               , HasDBConn r
               , MonadIO m
               )
            => Pagination
            -> OrderArg
            -> DatabaseEntity Postgres MyAppDb (TableEntity (AppEntity inner))
            -> m [AppEntity inner Identity]
getEntities pagination' orderArg t = do
  runQueryM $ paginateQuery pagination'
    $ orderBy_ (\e -> (desc_ (e ^. orderColumn)))
    $ all_ t
  where
    orderColumn =
      case orderArg of
        DefaultOrder -> updated_at
        CustomOrder _ -> updated_at


getEntity :: ( Beamable inner
               , Typeable inner
               , Generic (inner Identity)
               , Generic (inner Exposed)
               , GFromBackendRow Postgres (Rep (inner Exposed)) (Rep (inner Identity))
               , MonadReader r m
               , HasDBConn r
               , MonadIO m
               )
            => DatabaseEntity Postgres MyAppDb (TableEntity (AppEntity inner))
            -> UUID
            -> m (AppEntity inner Identity)
getEntity t uuid' = do
  result <- runQuerySingle $ select $
    do allItems <- (all_ t)
       guard_ (allItems ^. appId ==. val_ uuid')
       pure allItems
  return result
