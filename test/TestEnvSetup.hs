{-# LANGUAGE OverloadedStrings #-}

module TestEnvSetup where

import           App
import           AppPrelude
import           Init
import           Config.AppConfig
import qualified Database.PostgreSQL.Simple as PGS
import           Network.Wai


prepServer :: Config -> IO Application
prepServer config' = do
  runAppMigrations config'
  return $ app config'

truncateDatabase :: PGS.Connection -> IO ()
truncateDatabase conn = do
  let
    query'
      = "DO \n\
               \$func$ \n\
               \BEGIN \n\
               \  EXECUTE \n\
               \  (SELECT 'TRUNCATE TABLE ' \n\
               \       || string_agg(quote_ident(schemaname) || '.' || quote_ident(tablename), ', ') \n\
               \       || ' CASCADE' \n\
               \   FROM   pg_tables \n\
               \   WHERE  schemaname = 'public' \n\
               \   AND tablename NOT IN ('license_permission', 'business_permission') \n\
               \   ); \n\
               \END \n\
               \$func$;"
  _ <- liftIO (PGS.execute_ conn query')
  return ()
