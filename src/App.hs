{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module App where

import           AppPrelude                 hiding (traceIO)

import           Control.Lens.TH
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Crypto.JOSE.JWK            (JWK)
import           Data.Maybe                 (fromMaybe)
import           Data.Pool
import           Data.Text                  (pack, unpack)
import           Data.Text                  (Text)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Servant
import           System.Environment         (lookupEnv)
import           System.Log.FastLogger      (LoggerSet, pushLogStrLn, toLogStr)
import qualified System.Log.FastLogger      as FL
import           Text.Read                  (Read, readMaybe)

data Environment
  = Test
  | Development
  | Production
  deriving (Show, Eq, Read)

data LogTo
  = STDOut
  | STDErr
  | File Text
  deriving (Show, Eq, Read)

data Config = Config
  { _appLogger  :: LoggerSet
  , _appDBConn  :: DBConn
  , _appAuthKey :: JWK
  }

data DBConn = DBConn {
    _dbPoolConn :: Pool PGS.Connection
    }

makeLenses ''Config
makeClassy ''DBConn

newtype AppM a =
  AppM
  {runAppM :: ReaderT Config Servant.Handler a
  } deriving
    (Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Config
    , MonadError Servant.ServantErr
    )

instance HasDBConn Config where
  dBConn = appDBConn

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

envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8


lookupEnvStrDefault :: Text -> Text -> IO Text
lookupEnvStrDefault var def = do
  env <- lookupEnv . unpack $ var
  return (fromMaybe def (pack <$> env))


lookupEnvDefault :: Read a => Text -> a -> IO a
lookupEnvDefault var def = do
  env <- lookupEnv . unpack $ var
  return (fromMaybe def $ env >>= readMaybe)


lookupEnvOrError :: Text -> IO Text
lookupEnvOrError var = do
  env <- lookupEnv . unpack $ var
  case env of
    Just e  -> return $ pack e
    Nothing -> panic $ "Could not read environment variable: " <> var
