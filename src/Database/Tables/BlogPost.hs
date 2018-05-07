{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Tables.BlogPost where

import           AppPrelude
import           Control.Lens  hiding (element)
import           Data.Aeson
import           Data.UUID     (UUID, nil)
import           Database.Beam
import           GHC.Generics  (Generic)

type BlogPostID = UUID

data BlogPostT f
    = BlogPost
    { _blogPostId :: Columnar f BlogPostID
    , _title      :: Columnar f Text
    , _content    :: Columnar f Text
    } deriving (Generic)

type BlogPost = BlogPostT Identity

makeLenses ''BlogPostT

instance Beamable BlogPostT
instance Table BlogPostT where
  data PrimaryKey BlogPostT f = BlogPostID (Columnar f UUID) deriving Generic
  primaryKey = BlogPostID . _blogPostId

instance Beamable (PrimaryKey BlogPostT)
deriving instance Show BlogPost


instance ToJSON BlogPost where
    toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 1}

instance FromJSON BlogPost where
  parseJSON = withObject "blogPost" $ \o -> do
    _title <- o .: "title"
    _content <- o .: "content"
    let _blogPostId = nil
    return BlogPost{..}
