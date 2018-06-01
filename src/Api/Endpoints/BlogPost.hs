{-# LANGUAGE TemplateHaskell #-}

module Api.Endpoints.BlogPost where

import           Api.Resource
import           Config.AppConfig
import           AppPrelude
import           Data.Aeson
import           Models.User                         (UserResponse (..))
import           Data.UUID                (UUID)
import           Database.Crud
import           Database.MasterEntity
import           Database.Schema
import           Database.Tables.BlogPost
import           Pagination
import           Servant


type BlogPostViewAPI = RResourceAPI "blogPost" PaginatedResult BlogPostEntity UUID

blogPostViewServer :: ServerT BlogPostViewAPI AppM
blogPostViewServer = rResourceServer getBlogPosts getBlogPost

getBlogPosts :: (MonadIO m, MonadReader r m, HasDBConn r)
                => GetCollectionServer m PaginatedResult BlogPostEntity
getBlogPosts =
  getEntities blogPostTable


getBlogPost :: (MonadIO m, HasDBConn r, MonadReader r m) =>
                UUID -> m (AppEntity BlogPostBaseT Identity)
getBlogPost blogPostId' =
  getEntity blogPostTable blogPostId'

type BlogPostMutateAPI = CUDEntityAPI "blogPost" BlogPostEntity BlogPost

instance FromJSON BlogPost

blogPostMutateServer :: (HasDBConn r, MonadIO m, MonadReader r m)
                    => UserResponse
                    -> ServerT BlogPostMutateAPI m
blogPostMutateServer _ = cudEntityServer blogPostTable
