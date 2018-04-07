{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Api.Resource where

import           GHC.TypeLits
import           Servant


{- Read API -}
type RResourceAPI (resourceName :: Symbol) a i = resourceName :>
  (    Get '[JSON] [a]
  :<|> Capture "id" i    :> Get '[JSON] a
  )

cResourceServer
  :: Handler [a]
  -> (i -> Handler a)
  -> Server (RResourceAPI name a i)
cResourceServer listAs getA =
  listAs :<|> getA

{- Create/Read API -}
type CRResourceAPI (resourceName :: Symbol) a i =
       RResourceAPI resourceName a i
  :<|> ReqBody '[JSON] a :> Post '[JSON] NoContent

crResourceServer
  :: Handler [a]
  -> (i -> Handler a)
  -> (a -> Handler NoContent)
  -> Server (CRResourceAPI name a i)
crResourceServer listAs getA postA =
  (cResourceServer listAs getA) :<|> postA
