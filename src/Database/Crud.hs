{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Crud where

import           Api.Resource
import           AppPrelude
import           Config.AppConfig
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
import           Servant

queryTableCount
  :: (HasDBConn r, MonadReader r m, MonadIO m, Table table, Database be db)
  => DatabaseEntity be db (TableEntity table)
  -> m Int
queryTableCount entityTable = do
  let totalCount = aggregate_ (\_ -> as_ @Int countAll_) (all_ entityTable)
  runQuerySingle $ select totalCount
getEntities
  :: ( Beamable inner
     , Typeable inner
     , Generic (inner Identity)
     , Generic (inner Exposed)
     , Database.Beam.Backend.Types.GFromBackendRow
         Postgres
         (Rep (inner Exposed))
         (Rep (inner Identity))
     , MonadReader r m
     , HasDBConn r
     , MonadIO m
     )
  => DatabaseEntity Postgres MyAppDb (TableEntity (AppEntity inner))
  -> Maybe Limit
  -> Maybe Offset
  -> Maybe Order
  -> m (PaginatedResult (AppEntity inner Identity))
getEntities t mbLimit mbOffset mbOrder = do
  tableCount' <- queryTableCount t
  let pagination' = paramsToPagination mbLimit mbOffset
      paginationContext =
        getPaginationContext pagination' (TotalCount tableCount')
  data' <- runQueryM $ paginateQuery pagination' baseQuery
  return $ PaginatedResult {__data = data', __pagination = paginationContext}
 where
  baseQuery   = orderBy_ (\e -> (desc_ (e ^. orderColumn))) $ all_ t

  orderColumn = case mbOrder of
    Nothing -> updated_at
    Just _  -> updated_at


getEntity
  :: ( Beamable inner
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
  result <- runQuerySingle $ select $ do
    allItems <- (all_ t)
    guard_ (allItems ^. appId ==. val_ uuid')
    pure allItems
  return result


createEntity
  :: ( GFieldsFulfillConstraint
         (HasSqlValueSyntax PgValueSyntax)
         (Rep (table Exposed))
         (Rep (table Identity))
         (Rep (table (WithConstraint (HasSqlValueSyntax PgValueSyntax))))
     , Generic (table Exposed)
     , Generic (table Identity)
     , Generic (table (WithConstraint (HasSqlValueSyntax PgValueSyntax)))
     , Beamable table
     , MonadReader r m
     , HasDBConn r
     , GFromBackendRow Postgres (Rep (table Exposed)) (Rep (table Identity))
     , MonadIO m
     )
  => DatabaseEntity Postgres db (TableEntity (AppEntity table))
  -> table Identity
  -> m (AppEntity table Identity)
createEntity table' baseEntity = do
  now <- liftIO getCurrentTime
  res <-
    runInsertM table'
    $ insertExpressions
    $ [AppEntity default_ (val_ baseEntity) default_ (val_ now)]
  case res of
    (x : _) -> return x
    []      -> panic "No values returned on insert"

updateByID
  :: ( GFieldsFulfillConstraint
         (HasSqlValueSyntax PgValueSyntax)
         (Rep (table Exposed))
         (Rep (table Identity))
         (Rep (table (WithConstraint (HasSqlValueSyntax PgValueSyntax))))
     , Generic (table Exposed)
     , Generic (table Identity)
     , Generic (table (WithConstraint (HasSqlValueSyntax PgValueSyntax)))
     , Beamable table
     , MonadIO m
     , MonadReader r m
     , HasDBConn r
     )
  => DatabaseEntity be db (TableEntity (AppEntity table))
  -> UUID
  -> table Identity
  -> m ()
updateByID table' uuid' baseEntity' = do
  now <- liftIO getCurrentTime
  runSqlM $ runUpdate $ update
    table'
    (\u -> [u ^. baseTable <-. val_ baseEntity', u ^. updated_at <-. val_ now])
    (\u -> u ^. appId ==. val_ uuid')

deleteByID
  :: (MonadIO m, MonadReader r m, HasDBConn r)
  => DatabaseEntity be db (TableEntity (AppEntity table))
  -> UUID
  -> m ()
deleteByID table' uuid' =
  runSqlM $ runDelete $ delete table' (\u -> u ^. appId ==. val_ uuid')



type CrudEntityAPI (resourceName :: Symbol) a baseEntity = CRUDResourceAPI resourceName PaginatedResult a UUID baseEntity


crudEntityServer
  :: ( HasDBConn r
     , Generic (table Identity)
     , Generic (table Exposed)
     , Generic (table (WithConstraint (HasSqlValueSyntax PgValueSyntax)))
     , GFieldsFulfillConstraint
         (HasSqlValueSyntax PgValueSyntax)
         (Rep (table Exposed))
         (Rep (table Identity))
         (Rep (table (WithConstraint (HasSqlValueSyntax PgValueSyntax))))
     , Beamable table
     , Typeable table
     , GFromBackendRow Postgres (Rep (table Exposed)) (Rep (table Identity))
     , MonadIO m
     , MonadReader r m
     )
  => DatabaseEntity Postgres MyAppDb (TableEntity (AppEntity table))
  -> ServerT
       ( CRUDResourceAPI
           name
           PaginatedResult
           (AppEntity table Identity)
           UUID
           (table Identity)
       )
       m
crudEntityServer table' = crudResourceServer (getEntities table')
                                             (getEntity table')
                                             (createEntity table')
                                             (updateByID table')
                                             (deleteByID table')
