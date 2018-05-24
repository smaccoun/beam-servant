{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Database.Transaction where

import           App
import           AppPrelude
import           Control.Lens                  (view)
import           Database.Beam.Backend
import           Database.Beam.Postgres
import           Database.Beam.Query

runSql ::
       DBConn
    -> Pg a
    -> IO a
runSql pool query' = do
  conn <- getConnFromPool pool
  runBeamPostgres conn query'

runSqlM :: (MonadIO m, MonadReader r m, HasDBConn r)
    => Pg a
    -> m a
runSqlM query' = do
  conn <- view dBConn
  liftIO $ runSql conn query'


runQueryM :: (FromBackendRow Postgres a, HasDBConn r,
              MonadReader r m, MonadIO m) =>
              SqlSelect PgSelectSyntax a -> m [a]
runQueryM query' = do
  runSqlM $
    runSelectReturningList query'

runQuerySingle :: (MonadIO m, MonadReader r m, HasDBConn r,
                    FromBackendRow Postgres b) =>
                  SqlSelect PgSelectSyntax b -> m b
runQuerySingle query' = do
  result <- runSqlM $ runSelectReturningOne query'
  case result of
    Just x -> return x
    Nothing -> panic "Should have found exactly one result"


runInsertM :: (MonadIO m, MonadReader r m, HasDBConn r) =>
              SqlInsert PgInsertSyntax -> m ()
runInsertM insertStmt = do
  runSqlM (runInsert insertStmt)

