{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Database.Transaction where

import           App
import           AppPrelude
import           Database.Beam.Backend
import           Database.Beam.Postgres
import           Database.Beam.Query

runSql :: PGPool -> Pg a -> IO a
runSql pool query' = do
  conn <- getIOConnFromPool pool
  runBeamPostgres conn query'

runSqlM :: Pg a -> AppM a
runSqlM query' = do
  Config{..} <- ask
  liftIO $ runSql getPool query'


runQueryM :: (FromBackendRow Postgres a, MonadIO m,
              MonadReader Config m) =>
              SqlSelect PgSelectSyntax a -> m [a]
runQueryM query' = do
  Config{..} <- ask
  liftIO $ runSql getPool (runSelectReturningList query')


runQuerySingle :: (MonadReader Config m, MonadIO m,
                    FromBackendRow Postgres b) =>
                  SqlSelect PgSelectSyntax b -> m b
runQuerySingle query' = do
  result <- runQueryM query'
  case result of
    []    -> panic "No results found"
    [x]   -> return x
    (_:_) -> panic "More than one result found"

runInsertM :: (MonadIO m, MonadReader Config m) =>
              SqlInsert PgInsertSyntax -> m ()
runInsertM insertStmt = do
  Config{..} <- ask
  liftIO $ runSql getPool (runInsert insertStmt)
