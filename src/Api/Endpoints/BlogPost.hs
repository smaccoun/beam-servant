{-# LANGUAGE TemplateHaskell #-}

module Api.Endpoints.BlogPost where

import           Api.Resource
import           App
import           AppPrelude
import           Data.Aeson
import           Data.UUID                (UUID)
import           Database.Crud
import           Database.MasterEntity
import           Database.Schema
import           Database.Tables.BlogPost
import           Models.User              (UserResponse (..))
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

type BlogPostMutateAPI = "blogPost" :>
       CreateAPI BlogPost
  :<|> UpdateAPI BlogPost UUID

instance FromJSON BlogPost

blogPostMutateServer :: (HasDBConn r2, HasDBConn r1,
                          MonadReader r2 m2, MonadReader r1 m1, MonadIO m2, MonadIO m1) =>
                        UserResponse
                        -> (BlogPost -> m1 ())
                      :<|> (UUID -> BlogPost -> m2 ())
blogPostMutateServer _ =
       createBlogPost
  :<|> updateBlogPost

createBlogPost :: (MonadIO m, MonadReader r m, HasDBConn r)
                  => BlogPostBaseT Identity
                  -> m ()
createBlogPost bpr =
  createEntity blogPostTable bpr


updateBlogPost :: (MonadIO m, MonadReader r m, HasDBConn r)
                => UUID
                -> BlogPost
                -> m ()
updateBlogPost = updateByID blogPostTable

