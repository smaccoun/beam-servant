{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( startApp
    ) where

import           Control.Monad.Except                 (catchError)
import           Control.Monad.Trans.Reader           (runReaderT)
import           Control.Natural                      ((:~>) (NT))
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Servant                              as S
import qualified System.Log.FastLogger                as FL
import Servant.Auth.Server (generateKey, defaultJWTSettings, defaultCookieSettings)
import           App
import           AppPrelude
import qualified Data.Text                            as T

import Api.API
import           Config.AppConfig


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
    jwk <- generateKey
    midware   <- makeMiddleware logger env
    let initialLogMsg = intercalate " " $ ["Listening on port", show port, "at level", show env, "and logging to", show logTo, "with args", T.unpack (T.unwords args), "\n"]
    FL.pushLogStr logger $ FL.toLogStr initialLogMsg
    Warp.run port
      $ midware
      $ app (Config logger pool jwk)

app :: App.Config -> Wai.Application
app config@(Config _ _ authKey) = do
    let jwtCfg = defaultJWTSettings authKey
        cfg = defaultCookieSettings S.:. jwtCfg S.:. S.EmptyContext
    S.serveWithContext api cfg $ S.enter (NT $ runHandler config) (server jwtCfg)

runHandler :: Config -> AppM a -> S.Handler a
runHandler config handler =
  catchError (nt config handler) errorHandler
  where
    errorHandler :: S.ServantErr -> S.Handler a
    errorHandler err = errorHandler' err (S.errHTTPCode err)

    errorHandler' :: S.ServantErr -> Int -> S.Handler a
    errorHandler' err _ =
      S.throwError err

nt :: App.Config -> AppM a -> S.Handler a
nt s x = runReaderT x s
