module Api.Endpoints.BlogPost where

import           Api.Resource
import           App
import           AppPrelude
import           Control.Lens             hiding (element)
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
  runQueryM $ select (all_ blogPostTable)

getBlogPost :: BlogPostID -> AppM BlogPost
getBlogPost blogPostId' = do
  blogPostResult <- runQuerySingleM $
    select $
    do  blogPost <- (all_ blogPostTable)
        guard_ (blogPost ^. blogPostId ==. val_ blogPostId')
        pure blogPost
  return $ blogPostResult


type BlogPostMutateAPI = "blogPost" :>
  ( ReqBody '[JSON] BlogPost :> Post '[JSON] NoContent
  )

blogPostMutateServer ::
      UserResponse
  -> ServerT BlogPostMutateAPI AppM
blogPostMutateServer _ =
  createBlogPost

createBlogPost :: BlogPost -> AppM NoContent
createBlogPost blogPost = do
  _ <- runSqlM $ runInsert insertStmt
  return NoContent
  where
    insertStmt = insert blogPostTable $
        insertExpressions [ BlogPost default_ (val_ $ blogPost ^. title) (val_ $ blogPost ^. content)]
