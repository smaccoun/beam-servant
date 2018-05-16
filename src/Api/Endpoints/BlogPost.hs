{-# LANGUAGE TemplateHaskell #-}

module Api.Endpoints.BlogPost where

import           Api.Resource
import           App
import           AppPrelude
import           Control.Lens             hiding (element)
import           Data.Aeson
import           Data.Time.Clock          (getCurrentTime)
import           Database.Beam
import           Database.MasterEntity  (AppEntity (..), appId)
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
  runQueryM $ select (all_ blogPostTable)

getBlogPost :: UUID -> AppM BlogPostEntity
getBlogPost blogPostId' = do
  blogPostResult <- runQuerySingle $
    select $
    do  blogPost <- (all_ blogPostTable)
        guard_ (blogPost ^. appId ==. val_ blogPostId')
        pure blogPost
  return $ blogPostResult


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
