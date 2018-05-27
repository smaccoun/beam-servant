{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Api.API where

import           Api.Endpoints.BlogPost
import           Api.Endpoints.Login
import           Api.Endpoints.User
import           App
import           AppPrelude
import           Data.Swagger                        (Swagger, ToSchema)
import           Data.Swagger.Internal.ParamSchema
import           Data.Text                           (Text)
import           Database.Tables.BlogPost            (BlogPost, BlogPostEntity)
import           Models.Credentials                  (Email, Password)
import           Models.Login
import           Models.User                         (UserResponse (..))
import           Pagination
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import           Servant.Swagger

---------------------------------------------------------------
type Protected
   =    UserAPI
   :<|> BlogPostMutateAPI

protected :: AuthResult UserResponse -> ServerT Protected AppM
protected (Authenticated user) =
       userServer user
  :<|> blogPostMutateServer user

protected _ = throwAll err401

type Unprotected =
       "health" :> Get '[JSON] Text
  :<|> LoginAPI
  :<|> BlogPostViewAPI

unprotectedProxy :: Proxy Unprotected
unprotectedProxy = Proxy

unprotected :: JWTSettings -> ServerT Unprotected AppM
unprotected jwts =
       return "Okay"
  :<|> loginServer jwts
  :<|> blogPostViewServer

type API auths =
       (Auth auths UserResponse :> Protected)
  :<|> Unprotected

api :: Proxy (API '[JWT])
api = Proxy


serverAPI :: JWTSettings -> ServerT (API auths) AppM
serverAPI jwts =
       protected
  :<|> unprotected jwts


-- SWAGGER

swaggerUnprotected :: Swagger
swaggerUnprotected = toSwagger unprotectedProxy

instance ToSchema PaginationContext
instance (ToSchema a) => ToSchema (PaginatedResult a)
instance ToParamSchema Limit
instance ToParamSchema Offset
instance ToParamSchema Order

instance ToSchema Login
instance ToSchema Email
instance ToSchema LoginResponse
instance ToSchema Password
instance ToSchema BlogPostEntity
instance ToSchema BlogPost
