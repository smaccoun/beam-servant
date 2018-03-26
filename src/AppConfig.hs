{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AppConfig where

import Servant.API
import Data.Default
import qualified System.Log.FastLogger as FL
import qualified Servant                              as S
import           Control.Natural                      ((:~>) (NT))
import           Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Except (catchError)
import Data.Aeson
import           Network.Wai.Middleware.Cors
import qualified Network.Wai.Middleware.RequestLogger as MidRL
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp

import AppPrelude
import App
import Prelude (read)
import qualified Data.Text as T

makeMiddleware :: FL.LoggerSet -> Environment -> IO Wai.Middleware
makeMiddleware logger env =
          combineMiddleware corsified
        $ MidRL.mkRequestLogger
        $ def { MidRL.destination = MidRL.Logger logger }

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


data DBConfig =
  DBConfig
    {dbHost     :: T.Text
    ,dbPort     :: Int
    ,dbDatabase :: T.Text
    ,dbSchema   :: Maybe T.Text
    ,dbUsername :: T.Text
    ,dbPassword :: T.Text
    }


getDBConnectionInfo :: Environment -> IO DBConfig
getDBConnectionInfo _ = do
  dbHost'      <- lookupEnvOrError "DB_HOST"
  dbPort'      <- lookupEnvOrError "DB_PORT"
  dbDatabase'  <- lookupEnvOrError "DB_DATABASE"
  dbSchema'    <- lookupEnvOrError "DB_SCHEMA"
  dbUsername'  <- lookupEnvOrError "DB_USERNAME"
  dbPassword'  <- lookupEnvOrError "DB_PASSWORD"

  return $
    DBConfig
      {dbHost      = dbHost'
      ,dbPort      = read (T.unpack dbPort')
      ,dbDatabase  = dbDatabase'
      ,dbSchema    = Just dbSchema'
      ,dbUsername  = dbUsername'
      ,dbPassword  = dbPassword'
      }
