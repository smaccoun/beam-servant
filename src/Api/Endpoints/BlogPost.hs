module Api.Endpoints.BlogPost where

import           App
import           AppPrelude
import           Data.Aeson               (ToJSON)
import           Database.Beam
import           Database.Schema
import           Database.Tables.BlogPost
import           Database.Transaction
import           Models.User
import           Servant

type BlogPostAPI =
  "blogs" :>
    ( Get '[JSON] [BlogPost]
    )

instance ToJSON BlogPost

blogPostServer :: UserResponse -> ServerT BlogPostAPI AppM
blogPostServer _ = getBlogPosts


getBlogPosts :: AppM [BlogPost]
getBlogPosts = do
  result <- runQueryM $ select (all_ blogPostTable)
  return result
