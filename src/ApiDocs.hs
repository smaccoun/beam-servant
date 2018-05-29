module ApiDocs where

import           Api.API                 (unprotectedProxy)
import           Api.Endpoints.Login     (LoginResponse)
import           Data.Swagger            (Swagger, ToSchema)
import           Models.Credentials      (Email, Password)
import           Models.Login
import           Servant
import           Servant.Swagger
import qualified Servant.Swagger.UI      as SUI

type SwaggerAPI = SUI.SwaggerSchemaUI "swagger-ui" "swagger.json"

docServer :: Server SwaggerAPI
docServer =
  SUI.swaggerSchemaUIServer swaggerUnprotected


-- SWAGGER

swaggerUnprotected :: Swagger
swaggerUnprotected = toSwagger unprotectedProxy

instance ToSchema Login
instance ToSchema Email
instance ToSchema LoginResponse
instance ToSchema Password
