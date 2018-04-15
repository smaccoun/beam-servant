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


type BlogPostAPI = CRResourceAPI "blogPost" BlogPost BlogPostID

blogPostServer :: UserResponse -> ServerT BlogPostAPI AppM
blogPostServer _ = crResourceServer getBlogPosts getBlogPost createBlogPost

getBlogPosts :: AppM [BlogPost]
getBlogPosts = do
  result <- runQueryM $ select (all_ blogPostTable)
  return result

getBlogPost :: BlogPostID -> AppM BlogPost
getBlogPost blogPostId' = do
  blogPostResult <- runQuerySingleM $
    select $
    do  blogPost <- (all_ blogPostTable)
        guard_ (blogPost ^. blogPostId ==. val_ blogPostId')
        pure blogPost
  return $ blogPostResult


createBlogPost :: BlogPost -> AppM NoContent
createBlogPost blogPost = do
  _ <- runSqlM $ runInsert insertStmt
  return NoContent

  where
    insertStmt = insert blogPostTable $
        insertExpressions [ BlogPost default_ (val_ $ blogPost ^. title) (val_ $ blogPost ^. content)]
