{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

module Database.Transaction where

import           AppPrelude
import           Config.AppConfig
import           Control.Lens                             (view)
import           Database.Beam.Backend
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Query
import           Database.Beam.Schema.Tables
import           Init                                     (getConnFromPool)

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
    Just x  -> return x
    Nothing -> panic "Should have found exactly one result"


runInsertM :: (GFromBackendRow
                  Postgres (Rep (table Exposed)) (Rep (table Identity)),
                Generic (table Exposed), Generic (table Identity), Beamable table,
                HasDBConn r, MonadReader r m, MonadIO m) =>
              DatabaseEntity Postgres db (TableEntity table)
              -> SqlInsertValues
                    Database.Beam.Postgres.Syntax.PgInsertValuesSyntax
                    (table (QExpr Database.Beam.Postgres.Syntax.PgExpressionSyntax s))
              -> m [table Identity]
runInsertM table' insertStmt' = do
  runSqlM (runInsertReturningList table' insertStmt')

