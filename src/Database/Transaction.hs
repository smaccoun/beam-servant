{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Database.Transaction where

import           App
import           AppPrelude
import           Database.Beam.Backend
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Query
import           Database.Beam.Query.Internal

runSql :: PGPool -> Pg a -> IO a
runSql pool query' = do
  conn <- getIOConnFromPool pool
  runBeamPostgres conn query'

runSqlM :: (MonadReader Config m, MonadIO m) =>
     Pg a
  -> m a
runSqlM query' = do
  Config{..} <- ask
  liftIO $ runSql getPool query'


runQueryM :: (ThreadRewritable (QNested QueryInaccessible) a,
      ProjectibleWithPredicate ValueContext PgExpressionSyntax a,
      ProjectibleWithPredicate
        ValueContext
        PgExpressionSyntax
        (WithRewrittenThread
            (QNested QueryInaccessible) QueryInaccessible a),
      ProjectibleWithPredicate AnyType PgExpressionSyntax a,
      ProjectibleWithPredicate
        AnyType
        PgExpressionSyntax
        (WithRewrittenThread
            (QNested QueryInaccessible) QueryInaccessible a),
      FromBackendRow
        Postgres
        (QExprToIdentity
            (WithRewrittenThread
              (QNested QueryInaccessible) QueryInaccessible a)),
       MonadReader Config m,
       MonadIO m
             ) =>
         Maybe Limit
      -> Q PgSelectSyntax db (QNested QueryInaccessible) a
      -> m
          [QExprToIdentity
              (WithRewrittenThread
                (QNested QueryInaccessible) QueryInaccessible a)]
runQueryM mbLimit query' = do
  runSqlM $
    runSelectReturningList $ select $
      limit_ getLimit
        $ query'
  where
    getLimit =
      unLimit $ fromMaybe defaultLimit mbLimit

defaultLimit :: Limit
defaultLimit = Limit 10

newtype Limit = Limit {unLimit :: Integer}

runQuerySingle :: (FromBackendRow
          Postgres
          (QExprToIdentity
            (WithRewrittenThread
                (QNested QueryInaccessible) QueryInaccessible a)),
        ProjectibleWithPredicate
          AnyType
          PgExpressionSyntax
          (WithRewrittenThread
            (QNested QueryInaccessible) QueryInaccessible a),
        ProjectibleWithPredicate AnyType PgExpressionSyntax a,
        ProjectibleWithPredicate
          ValueContext
          PgExpressionSyntax
          (WithRewrittenThread
            (QNested QueryInaccessible) QueryInaccessible a),
        ProjectibleWithPredicate ValueContext PgExpressionSyntax a,
        ThreadRewritable (QNested QueryInaccessible) a,
       MonadReader Config m,
       MonadIO m
    ) =>
         Q PgSelectSyntax db (QNested QueryInaccessible) a
      -> m
            (QExprToIdentity
              (WithRewrittenThread
                  (QNested QueryInaccessible) QueryInaccessible a))
runQuerySingle query' = do
  result <- runQueryM (Just $ Limit 2) query'
  case result of
    []    -> panic "No results found"
    [x]   -> return x
    (_:_) -> panic "More than one result found"


runInsertM :: (MonadIO m, MonadReader Config m) =>
              SqlInsert PgInsertSyntax -> m ()
runInsertM insertStmt = do
  Config{..} <- ask
  liftIO $ runSql getPool (runInsert insertStmt)

