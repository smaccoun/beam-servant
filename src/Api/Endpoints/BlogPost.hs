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
       CreateAPI BlogPost BlogPostEntity
  :<|> UpdateAPI BlogPost UUID
  :<|> DeleteAPI UUID

instance FromJSON BlogPost

blogPostMutateServer :: (MonadIO m3, MonadIO m2, MonadIO m1,
                          MonadReader r3 m3, MonadReader r2 m2, MonadReader r1 m1,
                          HasDBConn r3, HasDBConn r2, HasDBConn r1) =>
                        p
                        -> (BlogPostBaseT Identity -> m1 BlogPostEntity)
                            :<|> ((UUID -> BlogPost -> m2 ()) :<|> (UUID -> m3 ()))
blogPostMutateServer _ =
       createBlogPost
  :<|> updateBlogPost
  :<|> deleteBlogPost

createBlogPost :: (MonadIO m, MonadReader r m, HasDBConn r)
                  => BlogPostBaseT Identity
                  -> m BlogPostEntity
createBlogPost bpr =
  createEntity blogPostTable bpr


updateBlogPost :: (MonadIO m, MonadReader r m, HasDBConn r)
                => UUID
                -> BlogPost
                -> m ()
updateBlogPost = updateByID blogPostTable


deleteBlogPost :: (HasDBConn r, MonadReader r m, MonadIO m) =>
                  UUID -> m ()
deleteBlogPost uuid' =
  deleteByID blogPostTable uuid'
