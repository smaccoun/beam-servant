{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Database.Tables.BlogPost where

import           AppPrelude
import           Data.Aeson
import           Data.Time.Clock
import           Data.UUID       (UUID)
import           Database.Beam
import           GHC.Generics    (Generic)

type BlogPostID = UUID

data BlogPostT f
    = BlogPost
    { blog_post_id :: Columnar f BlogPostID
    , title      :: Columnar f Text
    , content    :: Columnar f Text
    , updated_at  :: Columnar f UTCTime
    , created_at  :: Columnar f UTCTime
    } deriving (Generic)

type BlogPost = BlogPostT Identity


instance Beamable BlogPostT
instance Table BlogPostT where
  data PrimaryKey BlogPostT f = BlogPostID (Columnar f UUID) deriving Generic
  primaryKey = BlogPostID . blog_post_id

instance Beamable (PrimaryKey BlogPostT)
deriving instance Show BlogPost


instance ToJSON BlogPost where
    toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 1}

