{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Tables.BlogPost where

import           AppPrelude
import           Data.Aeson
import           Database.Beam
import           Database.MasterEntity
import           GHC.Generics          (Generic)

type BlogPostT = AppEntity BlogPostBaseT

data BlogPostBaseT f
    = BlogPostBaseT
    { title   :: Columnar f Text
    , content :: Columnar f Text
    } deriving (Generic)

instance Beamable BlogPostBaseT

type BlogPost = BlogPostBaseT Identity
type BlogPostEntity = BlogPostT Identity

instance ToJSON BlogPost
instance FromJSON BlogPost

