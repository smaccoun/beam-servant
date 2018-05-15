{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Schema where

import           AppPrelude
import           Control.Lens             hiding (element)
import           Database.Beam
import           Database.Tables.BlogPost
import           Database.Tables.User

data MyAppDb f =
  MyAppDb
    { _users     :: f (TableEntity UserT)
    , _blog_posts :: f (TableEntity BlogPostT)
    } deriving Generic

makeLenses ''MyAppDb

instance Database be MyAppDb

appDb :: DatabaseSettings be MyAppDb
appDb = defaultDbSettings


{- CONVENIENCE TABLE ACCESS -}

userTable :: DatabaseEntity be MyAppDb (TableEntity UserT)
userTable = appDb ^. users

blogPostTable :: DatabaseEntity be MyAppDb (TableEntity BlogPostT)
blogPostTable = appDb ^. blog_posts
