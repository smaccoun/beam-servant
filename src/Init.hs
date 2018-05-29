module Init where

import           AppPrelude                           hiding (traceIO)

import           Config.AppConfig
import           Control.Monad.IO.Class               (liftIO)
import           Data.Default
import           Data.Maybe                           (fromMaybe)
import           Data.Pool
import           Data.Text                            (pack, unpack)
import           Data.Text                            (Text)
import qualified Database.PostgreSQL.Simple           as PGS
import qualified Network.Wai                          as Wai
import           Network.Wai.Middleware.Cors
import qualified Network.Wai.Middleware.RequestLogger as MidRL
import           System.Environment                   (lookupEnv)
import           System.Log.FastLogger                (LoggerSet, pushLogStrLn,
                                                       toLogStr)
import qualified System.Log.FastLogger                as FL
import           Text.Read                            (Read, readMaybe)

addToLogger :: Text -> AppM ()
addToLogger message =
  AppPrelude.ask >>= \cfg -> liftIO $ pushLogStrLn (_appLogger cfg) (toLogStr message)

makeLogger :: LogTo -> IO LoggerSet
makeLogger logTo = case logTo of
        STDOut        -> FL.newStdoutLoggerSet FL.defaultBufSize
        STDErr        -> FL.newStderrLoggerSet FL.defaultBufSize
        File filename -> FL.newFileLoggerSet FL.defaultBufSize $ unpack filename

getConnFromPool :: MonadIO m => DBConn -> m PGS.Connection
getConnFromPool (DBConn pool) = liftIO $ withResource pool return


mkPool :: PGS.ConnectInfo -> IO (Pool PGS.Connection)
mkPool con =
  createPool start PGS.close 10 10 5
    where
      start = PGS.connect con

makeMiddleware :: FL.LoggerSet -> Environment -> IO Wai.Middleware
makeMiddleware logger _ =
          combineMiddleware corsified
        $ MidRL.mkRequestLogger
        $ Data.Default.def { MidRL.destination = MidRL.Logger logger }

corsified :: Wai.Middleware
corsified = cors (const $ Just appCorsResourcePolicy)

combineMiddleware :: Wai.Middleware -> IO Wai.Middleware -> IO Wai.Middleware
combineMiddleware a = fmap (. a)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy = CorsResourcePolicy {
    corsOrigins        = Nothing
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
envPool Production  = 8


lookupEnvStrDefault :: Text -> Text -> IO Text
lookupEnvStrDefault var def' = do
  env <- lookupEnv . unpack $ var
  return (fromMaybe def' (pack <$> env))


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
