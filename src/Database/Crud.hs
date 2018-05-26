{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Crud where

import           App
import           AppPrelude
import           Control.Lens                    hiding (element)
import           Data.Time.Clock
import           Data.UUID                       (UUID)
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Backend.Types
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Schema.Tables
import           Database.MasterEntity
import           Database.Schema
import           Database.Transaction
import           GHC.Generics                    (Generic)
import           Pagination
import           Pagination                      (paramsToPagination)

data OrderArg = DefaultOrder | CustomOrder Text

queryTableCount :: (HasDBConn r, MonadReader r m, MonadIO m,
                          Table table, Database be db) =>
                          DatabaseEntity be db (TableEntity table) -> m Int
queryTableCount entityTable = do
  let totalCount = aggregate_ (\_ -> as_ @Int countAll_) (all_ entityTable)
  runQuerySingle $ select totalCount

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
            => Maybe Limit
            -> Maybe Offset
            -> OrderArg
            -> DatabaseEntity Postgres MyAppDb (TableEntity (AppEntity inner))
            -> m (PaginatedResult (AppEntity inner Identity))
getEntities mbLimit mbOffset orderArg t = do
  tableCount' <- queryTableCount t
  let pagination' = paramsToPagination mbLimit mbOffset
      paginationContext = getPaginationContext pagination' (TotalCount tableCount')
  data' <- runQueryM $ paginateQuery pagination' baseQuery
  return $
    PaginatedResult
      {__data = data'
      ,__pagination = paginationContext
      }
  where
    baseQuery =
      orderBy_ (\e -> (desc_ (e ^. orderColumn)))
      $ all_ t

    orderColumn =
      case orderArg of
        DefaultOrder  -> updated_at
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


--createEntity :: (MonadIO m, MonadReader r m, HasDBConn r, Beamable table)
--             => p
--             -> DatabaseEntity be db (TableEntity (AppEntity table))
--             -> m ()
createEntity :: (Database.Beam.Schema.Tables.GFieldsFulfillConstraint
                  (Database.Beam.Backend.SQL.SQL92.HasSqlValueSyntax PgValueSyntax)
                  (Rep (table Exposed))
                  (Rep (table Identity))
                  (Rep
                    (table (Database.Beam.Schema.Tables.WithConstraint
                              (Database.Beam.Backend.SQL.SQL92.HasSqlValueSyntax
                                  PgValueSyntax)))),
                Generic (table Exposed), Generic (table Identity),
                Generic
                  (table (Database.Beam.Schema.Tables.WithConstraint
                            (Database.Beam.Backend.SQL.SQL92.HasSqlValueSyntax
                              PgValueSyntax))),
                Beamable table, HasDBConn r, MonadReader r m, MonadIO m)
              => DatabaseEntity be db (TableEntity (AppEntity table))
              -> table Identity
              -> (m) ()
createEntity table' baseEntity = do
  now <- liftIO $ getCurrentTime
  runInsertM $
      insert table' $
            insertExpressions
              [ AppEntity
                  default_
                  (val_ baseEntity)
                  default_
                  (val_ now)
              ]
