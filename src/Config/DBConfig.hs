{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Config.DBConfig where

import           AppPrelude
import           Control.Lens.TH
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PGS

data DBConfig =
  DBConfig
    {dbHost     :: T.Text
    ,dbPort     :: Integer
    ,dbDatabase :: T.Text
    ,dbSchema   :: Maybe T.Text
    ,dbUsername :: T.Text
    ,dbPassword :: T.Text
    } deriving (Show)

makeClassy ''DBConfig

connInfoToPG :: DBConfig -> PGS.ConnectInfo
connInfoToPG (DBConfig dbHost dbPort dbDatabase _ dbUsername dbPassword) =
  PGS.defaultConnectInfo { PGS.connectHost     = T.unpack dbHost
                         , PGS.connectUser     = T.unpack dbUsername
                         , PGS.connectPort     = fromInteger dbPort
                         , PGS.connectPassword = T.unpack dbPassword
                         , PGS.connectDatabase = T.unpack dbDatabase
                         }
