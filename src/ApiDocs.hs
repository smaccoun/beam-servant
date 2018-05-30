module ApiDocs where

import           Api.API                           (unprotectedProxy)
import           Api.Endpoints.Login               (LoginResponse)
import           Data.Swagger                      (Swagger, ToSchema)
import           Data.Swagger.Internal.ParamSchema
import           Models.Credentials                (Email, Password)
import           Models.Login
import           Pagination
import           Servant
import           Servant.Swagger
import qualified Servant.Swagger.UI                as SUI

type SwaggerAPI = SUI.SwaggerSchemaUI "swagger-ui" "swagger.json"

docServer :: Server SwaggerAPI
docServer = SUI.swaggerSchemaUIServer swaggerUnprotected


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
