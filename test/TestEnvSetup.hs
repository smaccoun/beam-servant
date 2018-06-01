{-# LANGUAGE OverloadedStrings #-}

module TestEnvSetup where

import           App
import           AppPrelude
import           Init
import           Config.AppConfig
import qualified Database.PostgreSQL.Simple as PGS
import           Database.PostgreSQL.Simple.Migration
import           Network.Wai

prepServer :: Config -> IO Application
prepServer config' = do
  let dbConn' = _appDBConn config'
  putStrLn ("Meow" :: Text)
  conn <- getConnFromPool dbConn'
  putStrLn ("GOT CONN" :: Text)
  _ <- PGS.withTransaction conn $ runMigration $ MigrationContext
    MigrationInitialization
    True
    conn
  putStrLn ("iNITIMIAL MIGRATION" :: Text)
  _ <- PGS.withTransaction conn $ runMigration $ MigrationContext
    (MigrationDirectory "./migrations")
    True
    conn
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
