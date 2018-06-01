module Init where

import           AppPrelude                           hiding (traceIO)

import           Config.AppConfig
import           Config.DBConfig
import           Control.Monad.IO.Class               (liftIO)
import           Data.Default
import           Data.Maybe                           (fromMaybe)
import           Data.Pool
import           Data.Text                            (pack, unpack)
import           Data.Text                            (Text)
import qualified Database.PostgreSQL.Simple           as PGS
import           Database.PostgreSQL.Simple.Migration
import qualified Network.Wai                          as Wai
import           Network.Wai.Middleware.Cors
import qualified Network.Wai.Middleware.RequestLogger as MidRL
import           Prelude                              (read)
import           System.Environment                   (lookupEnv)
import           System.Log.FastLogger                (LoggerSet, pushLogStrLn,
                                                       toLogStr)
import qualified System.Log.FastLogger                as FL
import           Text.Read                            (Read, readMaybe)

data EnvConfig =
  EnvConfig
    {runEnv      :: Environment
    ,port        :: Int
    ,dbEnvConfig :: DBConfig
    }

readEnv :: IO EnvConfig
readEnv = do
  runEnv    <- lookupEnvDefault "SERVANT_ENV" Development
  port      <- lookupEnvDefault "SERVANT_PORT" 8080
  dbConfig' <- getDBConnectionInfo

  return $ EnvConfig {runEnv = runEnv, port = port, dbEnvConfig = dbConfig'}

makeLogger :: LogTo -> IO LoggerSet
makeLogger logTo = case logTo of
  STDOut        -> FL.newStdoutLoggerSet FL.defaultBufSize
  STDErr        -> FL.newStderrLoggerSet FL.defaultBufSize
  File filename -> FL.newFileLoggerSet FL.defaultBufSize $ unpack filename

mkPool :: PGS.ConnectInfo -> IO (Pool PGS.Connection)
mkPool con = createPool start PGS.close 10 10 5 where start = PGS.connect con

makeMiddleware :: FL.LoggerSet -> Environment -> IO Wai.Middleware
makeMiddleware logger _ =
  combineMiddleware corsified $ MidRL.mkRequestLogger $ Data.Default.def
    { MidRL.destination = MidRL.Logger logger
    }



addToLogger :: Text -> AppM ()
addToLogger message = AppPrelude.ask
  >>= \cfg -> liftIO $ pushLogStrLn (_appLogger cfg) (toLogStr message)

getConnFromPool :: MonadIO m => DBConn -> m PGS.Connection
getConnFromPool (DBConn pool) = liftIO $ withResource pool return


corsified :: Wai.Middleware
corsified = cors (const $ Just appCorsResourcePolicy)

combineMiddleware :: Wai.Middleware -> IO Wai.Middleware -> IO Wai.Middleware
combineMiddleware a = fmap (. a)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy = CorsResourcePolicy
  { corsOrigins        = Nothing
  , corsMethods        = ["OPTIONS", "GET", "PUT", "POST", "PATCH", "DELETE"]
  , corsRequestHeaders = ["Authorization", "Content-Type"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
  }

envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 10

runAppMigrations :: Config -> IO ()
runAppMigrations config' = do
  let dbConn' = _appDBConn config'
  conn <- getConnFromPool dbConn'
  _ <- PGS.withTransaction conn $ runMigration $ MigrationContext
    MigrationInitialization
    True
    conn
  _ <- PGS.withTransaction conn $ runMigration $ MigrationContext
    (MigrationDirectory migrationDir)
    True
    conn
  return ()
  where
    migrationDir = "./migrations"



lookupEnvDefault :: Read a => Text -> a -> IO a
lookupEnvDefault var def' = do
  env <- lookupEnv . unpack $ var
  return (fromMaybe def' $ env >>= readMaybe)


lookupEnvOrError :: Text -> IO Text
lookupEnvOrError var = do
  env <- lookupEnv . unpack $ var
  case env of
    Just e  -> return $ pack e
    Nothing -> panic $ "Could not read environment variable: " <> var


getDBConnectionInfo :: IO DBConfig
getDBConnectionInfo = do
  dbHost'     <- lookupEnvOrError "DB_HOST"
  dbPort'     <- lookupEnvOrError "DB_PORT"
  dbDatabase' <- lookupEnvOrError "DB_DATABASE"
  dbSchema'   <- lookupEnvOrError "DB_SCHEMA"
  dbUsername' <- lookupEnvOrError "DB_USERNAME"
  dbPassword' <- lookupEnvOrError "DB_PASSWORD"

  return $ DBConfig
    { dbHost     = dbHost'
    , dbPort     = read (unpack dbPort')
    , dbDatabase = dbDatabase'
    , dbSchema   = Just dbSchema'
    , dbUsername = dbUsername'
    , dbPassword = dbPassword'
    }
