{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Config.AppConfig where


import           AppPrelude                 hiding (traceIO)

import           Control.Lens.TH
import           Control.Monad.Trans.Reader (ReaderT)
import           Crypto.JOSE.JWK            (JWK)
import           Data.Pool
import qualified Database.PostgreSQL.Simple as PGS
import qualified Servant
import           System.Log.FastLogger      (LoggerSet)
import           Text.Read                  (Read)


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

data Config = Config
  { _appLogger  :: LoggerSet
  , _appDBConn  :: DBConn
  , _appAuthKey :: JWK
  }

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


data DBConn = DBConn {
    _dbPoolConn :: Pool PGS.Connection
    }

makeLenses ''Config
makeClassy ''DBConn

instance HasDBConn Config where
  dBConn = appDBConn

