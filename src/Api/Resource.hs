{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Api.Resource where

import           GHC.TypeLits
import           Servant

{- Create/Read API -}
type CRResourceAPI (resourceName :: Symbol) a i = resourceName :>
  (                         Get '[JSON] [a]
  :<|> Capture "id" i    :> Get '[JSON] a
  :<|> ReqBody '[JSON] a :> Post '[JSON] NoContent
  )

crResourceServer
  :: Handler [a]
  -> (i -> Handler a)
  -> (a -> Handler NoContent)
  -> Server (CRResourceAPI name a i)
crResourceServer listAs getA postA =
  listAs :<|> getA :<|> postA
