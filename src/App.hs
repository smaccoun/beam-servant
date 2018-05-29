{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module App where

import           AppPrelude               hiding (traceIO)

import           Api.API
import           Config.AppConfig
import           Config.DBConfig
import           Crypto.JOSE.JWK          (JWK)
import qualified Data.Aeson               as A
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Init
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Prelude                  (String)
import           Servant
import qualified Servant                  as S
import           Servant.Auth.Server      (CookieSettings (..), JWT,
                                           JWTSettings, defaultCookieSettings,
                                           defaultJWTSettings)
import qualified System.Log.FastLogger    as FL
import ApiDocs

startApp :: [[Char]] -> IO ()
startApp charArgs = do
    let args = fmap T.pack charArgs
    runEnv  <- lookupEnvDefault "SERVANT_ENV" Development
    port <- lookupEnvDefault "SERVANT_PORT" 8080
    dbConfig' <- getDBConnectionInfo

    (config', logTo) <- setAppConfig runEnv dbConfig' args
    let logger = _appLogger config'

    midware <- makeMiddleware logger runEnv
    FL.pushLogStr logger $ FL.toLogStr $ logInitialMetaInfo port runEnv logTo args

    startServer port midware config'


startServer :: Warp.Port
                -> (Application
                -> Application)
                -> Config
                -> IO ()
startServer port midware config' =
    Warp.run port
      $ midware
      $ app config'


setAppConfig :: Environment -> DBConfig -> [Text] -> IO (Config, LogTo)
setAppConfig _ dbConfig' args = do
    pool <- getDBConnection dbConfig'
    logTo <- case listToMaybe args of
      Just filename -> return $ File filename
      Nothing       -> lookupEnvDefault "SERVANT_LOG" STDOut
    logger  <- makeLogger logTo

    jwkJsonString <- fmap (LBS.fromStrict . encodeUtf8) $ lookupEnvOrError "AUTH_JWK"
    let eitherJWKJson = (A.eitherDecode jwkJsonString :: Either String JWK)
        jwk =
          case eitherJWKJson of
            Right j -> j
            Left e  -> panic $ "BAD JWK: " <> T.pack e <> ". From reading: " <> (decodeUtf8 $ LBS.toStrict jwkJsonString)

    return (Config logger pool jwk, logTo)

getDBConnection :: DBConfig -> IO DBConn
getDBConnection dbConfig' = do
    fmap DBConn $ mkPool $ connInfoToPG dbConfig'


app :: Config -> Wai.Application
app config'@(Config _ _ authKey) = do
    let jwtCfg = defaultJWTSettings authKey
        cfg = defaultCookieSettings S.:. jwtCfg S.:. S.EmptyContext
        apiWithDocs = (Proxy :: Proxy ApiWithDocs)
    S.serveWithContext apiWithDocs cfg (serverWithDocs config' jwtCfg)

appServer :: Config -> JWTSettings ->  S.Server (API auths)
appServer config' jwtConfig =
  let c = Proxy :: Proxy '[CookieSettings, JWTSettings]
  in
  S.hoistServerWithContext api c (runCustomHandler config') (serverAPI jwtConfig)

type ApiWithDocs =
       API '[JWT]
  :<|> SwaggerAPI


serverWithDocs :: Config -> JWTSettings ->  S.Server ApiWithDocs
serverWithDocs config' jwtConfig =
       appServer config' jwtConfig
  :<|> docServer


runCustomHandler :: Config -> AppM a -> S.Handler a
runCustomHandler config' handler =
  catchError (nt config' handler) errorHandler
  where
    errorHandler :: S.ServantErr -> S.Handler a
    errorHandler err = errorHandler' err (S.errHTTPCode err)

    errorHandler' :: S.ServantErr -> Int -> S.Handler a
    errorHandler' err _ =
      S.throwError err

nt :: Config -> AppM a -> S.Handler a
nt s x = runReaderT (runAppM x) s



logInitialMetaInfo :: (Show a, Show a1, Show a2) =>
                a2 -> a1 -> a -> [Text] -> [Char]
logInitialMetaInfo port env logTo args =
  intercalate " " $
      ["Listening on port", show port
      , "at level", show env
      , "and logging to", (show logTo)
      , "with args", T.unpack (T.unwords args)
      , "\n"
      ]
