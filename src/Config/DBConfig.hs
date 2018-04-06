{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Config.DBConfig where

import           App
import           AppPrelude
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PGS
import           Prelude                    (read)

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


connInfoToPG :: DBConfig -> PGS.ConnectInfo
connInfoToPG connInfo = PGS.defaultConnectInfo
                        { PGS.connectHost = T.unpack . dbHost $ connInfo
                        , PGS.connectUser = T.unpack . dbUsername $ connInfo
                        , PGS.connectPassword = T.unpack . dbPassword $ connInfo
                        , PGS.connectDatabase = T.unpack . dbDatabase $ connInfo
                        }
