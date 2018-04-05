{-# LANGUAGE ConstraintKinds #-}

module Database.Transaction where

import           App
import           AppPrelude
import           Database.Beam              (MonadBeam, withDatabase)
import qualified Database.PostgreSQL.Simple as PGS

type MonadQuery syntax be m = MonadBeam syntax be PGS.Connection m

runQuery ::  MonadQuery syntax be m => PGConn -> m a -> IO a
runQuery pool query' = do
  conn <- getIOConnFromPool pool
  withDatabase conn query'

runQueryM :: MonadQuery syntax be m => m a -> AppM a
runQueryM query' = do
  conn <- getConn
  liftIO $ withDatabase conn query'
