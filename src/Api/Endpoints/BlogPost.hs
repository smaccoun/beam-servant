{-# LANGUAGE TemplateHaskell #-}

module Api.Endpoints.BlogPost where

import           Api.Resource
import           App
import           AppPrelude
import           Control.Lens             hiding (element)
import           Data.Aeson
import           Data.Time.Clock          (getCurrentTime)
import           Database.Beam
import           Database.Schema
import           Database.Tables.BlogPost
import           Database.Transaction
import           Models.User
import           Servant


type BlogPostViewAPI = RResourceAPI "blogPost" BlogPost BlogPostID

blogPostViewServer :: ServerT BlogPostViewAPI AppM
blogPostViewServer = rResourceServer getBlogPosts getBlogPost

getBlogPosts :: AppM [BlogPost]
getBlogPosts = do
  runSqlDebugM $ runSelectReturningList $ select (all_ blogPostTable)

getBlogPost :: BlogPostID -> AppM BlogPost
getBlogPost blogPostId' = do
  blogPostResult <- runQuerySingleM $
    select $
    do  blogPost <- (all_ blogPostTable)
        guard_ (blog_post_id blogPost ==. val_ blogPostId')
        pure blogPost
  return $ blogPostResult


type BlogPostMutateAPI = "blogPost" :>
  ( ReqBody '[JSON] BlogPostRequest :> Post '[JSON] NoContent
  )

data BlogPostRequest
    = BlogPostRequest
    { _title'   :: Text
    , _content' :: Text
    } deriving (Generic)

makeLenses ''BlogPostRequest

instance FromJSON BlogPostRequest

blogPostMutateServer ::
      UserResponse
  -> ServerT BlogPostMutateAPI AppM
blogPostMutateServer _ =
  createBlogPost

createBlogPost :: BlogPostRequest -> AppM NoContent
createBlogPost bpr = do
  now <- liftIO getCurrentTime
  _ <- runSqlM $ runInsert $ insertStmt now
  return NoContent
  where
    insertStmt now = insert blogPostTable $
        insertExpressions
          [ BlogPost
              default_
              (val_ $ bpr ^. title')
              (val_ $ bpr ^. content')
              default_
              (val_ now)
          ]
