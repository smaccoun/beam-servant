{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( startApp
    ) where

import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.API
import Data.Aeson

import AppPrelude
import qualified Data.Text as T


type API = Get '[JSON] Int

api :: Proxy API
api = Proxy

server :: Server API
server = return 10

startApp :: IO ()
startApp = do
    Warp.run 8080 app

app :: Application
app =
    serve api server

