{-# LANGUAGE TemplateHaskell #-}

module Api.Endpoints.BlogPost where

import           Api.Resource
import           App
import           AppPrelude
import           Data.Aeson
import           Data.Time.Clock          (getCurrentTime)
import           Database.Beam
import           Database.Crud
import           Database.MasterEntity  (AppEntity (..))
import           Database.Schema
import           Data.UUID              (UUID)
import           Database.Tables.BlogPost
import           Database.Transaction
import           Models.User
import           Servant


type BlogPostViewAPI = RResourceAPI "blogPost" BlogPostEntity UUID

blogPostViewServer :: ServerT BlogPostViewAPI AppM
blogPostViewServer = rResourceServer getBlogPosts getBlogPost

getBlogPosts :: AppM [BlogPostEntity]
getBlogPosts = do
  getEntities blogPostTable

getBlogPost :: UUID -> AppM BlogPostEntity
getBlogPost blogPostId' = do
  getEntity blogPostTable blogPostId'

type BlogPostMutateAPI = "blogPost" :>
  ( ReqBody '[JSON] BlogPost :> Post '[JSON] NoContent
  )

instance FromJSON BlogPost

blogPostMutateServer ::
      UserResponse
  -> ServerT BlogPostMutateAPI AppM
blogPostMutateServer _ =
  createBlogPost

createBlogPost :: BlogPost -> AppM NoContent
createBlogPost bpr = do
  now <- liftIO getCurrentTime
  _ <- runInsertM $ insertStmt now
  return NoContent
  where
    insertStmt now = insert blogPostTable $
        insertExpressions
          [ AppEntity
              default_
              (BlogPostBaseT (val_ $ title bpr) (val_ $ content bpr))
              default_
              (val_ now)
          ]
