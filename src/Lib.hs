{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( startApp
    ) where

import           Control.Monad.Except                 (catchError)
import           Control.Monad.Trans.Reader           (runReaderT)
import           Control.Natural                      ((:~>) (NT))
import           Data.Aeson
import           Data.Default
import           Data.Pool
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.Cors
import qualified Network.Wai.Middleware.RequestLogger as MidRL
import qualified Servant                              as S
import           Servant.API
import qualified System.Log.FastLogger                as FL

import           App
import           AppPrelude
import qualified Data.Text                            as T

import           Api.User
import           Config.AppConfig
import Database.Beam

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
      Nothing       -> lookupEnvDefault "SERVANT_LOG" STDOut

    logger  <- makeLogger logTo
    dbConfig <- getDBConnectionInfo  env
    pool <- mkPool $ connInfoToPG dbConfig
    midware   <- makeMiddleware logger env
    let initialLogMsg = intercalate " " $ ["Listening on port", show port, "at level", show env, "and logging to", show logTo, "with args", T.unpack (T.unwords args), "\n"]
    FL.pushLogStr logger $ FL.toLogStr initialLogMsg
    Warp.run port
      $ midware
      $ app (Config logger pool)

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
