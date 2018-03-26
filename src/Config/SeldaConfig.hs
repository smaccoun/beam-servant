module Config.SeldaConfig where

import AppPrelude
import Config.AppConfig
import Database.Selda.PostgreSQL

dbConfigToSeldaPGConfig :: DBConfig -> PGConnectInfo
dbConfigToSeldaPGConfig (DBConfig dbHost dbPort dbDatabase dbSchema dbUsername dbPassword) =
  PGConnectInfo
    {pgHost = dbHost
    ,pgPort = dbPort
    ,pgDatabase = dbDatabase
    ,pgSchema = dbSchema
    ,pgUsername = Just dbUsername
    ,pgPassword = Just dbPassword
    }

