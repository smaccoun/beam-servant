{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Schema where

import           AppPrelude
import           Control.Lens         hiding (element)
import           Database.Beam
import           Database.Tables.User

data MyAppDb f =
  MyAppDb
    { _users :: f (TableEntity UserT)
    } deriving Generic

makeLenses ''MyAppDb

instance Database be MyAppDb

appDb :: DatabaseSettings be MyAppDb
appDb = defaultDbSettings `withDbModification`
            dbModification {
              _users = modifyTable (\a -> a) $
                        tableModification {
                          _user =
                            tableModification
                               { _email = fieldNamed "email"
                               , _password = fieldNamed "password"
                                }
                        }
            }


{- CONVENIENCE TABLE ACCESS -}

userTable :: DatabaseEntity be MyAppDb (TableEntity UserT)
userTable = appDb ^. users
