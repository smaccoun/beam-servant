module Api.Endpoints.BlogPost where

import           Api.Resource
import           App
import           AppPrelude
import           Control.Lens             hiding (element)
import           Data.Aeson               (ToJSON)
import           Database.Beam
import           Database.Schema
import           Database.Tables.BlogPost
import           Database.Transaction
import           Models.User
import           Servant


type BlogPostAPI = RResourceAPI "blogPost" BlogPost BlogPostID

instance ToJSON BlogPost

blogPostServer :: UserResponse -> ServerT BlogPostAPI AppM
blogPostServer _ = rResourceServer getBlogPosts getBlogPost


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

