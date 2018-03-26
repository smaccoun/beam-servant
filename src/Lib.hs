{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( startApp
    ) where

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
import qualified Data.Text as T

import AppConfig
import Api.User


type API =
       Get '[JSON] T.Text
  :<|> UserAPI

api :: Proxy API
api = Proxy

server :: S.ServerT API AppM
server =
       return "hello world"
  :<|> userServer

startApp :: [[Char]] -> IO ()
startApp charArgs = do
    let args = fmap T.pack charArgs
    env  <- lookupEnvDefault "SERVANT_ENV" Development
    port <- lookupEnvDefault "SERVANT_PORT" 8080

    logTo <- case listToMaybe args of
      Just filename -> return $ File filename
      Nothing -> lookupEnvDefault "SERVANT_LOG" STDOut

    logger  <- makeLogger logTo
    midware   <- makeMiddleware logger env
    dbConfig <- getDBConnectionInfo  env
    let initialLogMsg = intercalate " " ["Listening on port", show port, "at level", show env, "and logging to", show logTo, "with args", T.unpack (T.unwords args), "\n"]
    FL.pushLogStr logger $ FL.toLogStr initialLogMsg
    Warp.run port
      $ midware
      $ app (Config logger)

app :: App.Config -> Wai.Application
app config = do
    let api = S.Proxy :: S.Proxy API
    S.serve api $ S.enter (NT $ runHandler config) server

runHandler :: Config -> AppM a -> S.Handler a
runHandler config handler =
  catchError (nt config handler) errorHandler
  where
    errorHandler :: S.ServantErr -> S.Handler a
    errorHandler err = errorHandler' err (S.errHTTPCode err)

    errorHandler' :: S.ServantErr -> Int -> S.Handler a
    errorHandler' err code =
      S.throwError err

nt :: App.Config -> AppM a -> S.Handler a
nt s x = runReaderT x s

