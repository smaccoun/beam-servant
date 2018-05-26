{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Api.Resource where

import           AppPrelude
import           Pagination
import           Servant


type GetListAPI c a =
  QueryParam "limit" Limit
    :> QueryParam "page" Offset
    :> QueryParam "orderBy" Text
    :> Get '[JSON] (c a)

type GetItemAPI a i =
  Capture "id" i    :> Get '[JSON] a

type GetCollectionServer m c a = (Maybe Limit -> Maybe Offset -> Maybe Text -> m (c a))

{- Read API -}
type RResourceAPI (resourceName :: Symbol) c a i = resourceName :>
  (    GetListAPI c a
  :<|> GetItemAPI a i
  )

rResourceServer ::
     GetCollectionServer m c a
  -> (i -> m a)
  -> ServerT (RResourceAPI name c a i) m
rResourceServer listAs getA =
  listAs :<|> getA


{- Create/Read API -}

type CreateAPI a =
   ReqBody '[JSON] a :> Post '[JSON] NoContent

type CRResourceAPI (resourceName :: Symbol) c a i = resourceName :>
  (    GetListAPI c a
  :<|> GetItemAPI a i
  :<|> CreateAPI a
  )

crResourceServer
  :: GetCollectionServer m c a
  -> (i -> m a)
  -> (a -> m NoContent)
  -> ServerT (CRResourceAPI name c a i) m
crResourceServer listAs getA postA =
  listAs :<|> getA :<|> postA


{- Create/Read/Update API -}
type UpdateAPI a =
   ReqBody '[JSON] a :> Patch '[JSON] NoContent

type CRUResourceAPI (resourceName :: Symbol) c a i = resourceName :>
  (    GetListAPI c a
  :<|> GetItemAPI a i
  :<|> CreateAPI a
  :<|> UpdateAPI a
  )

cruResourceServer
  :: GetCollectionServer m c a
  -> (i -> m a)
  -> (a -> m NoContent)
  -> (a -> m NoContent)
  -> ServerT (CRUResourceAPI name c a i) m
cruResourceServer listAs getA postA updateA =
  listAs :<|> getA :<|> postA :<|> updateA

{- Create/Read/Update/Delete API -}
type DeleteAPI a i =
   Capture "id" i :> Delete '[JSON] NoContent

type CRUDResourceAPI (resourceName :: Symbol) c a i = resourceName :>
  (    GetListAPI c a
  :<|> GetItemAPI a i
  :<|> CreateAPI a
  :<|> UpdateAPI a
  :<|> DeleteAPI a i
  )

crudResourceServer
  :: GetCollectionServer m c a
  -> (i -> m a)
  -> (a -> m NoContent)
  -> (a -> m NoContent)
  -> (i -> m NoContent)
  -> ServerT (CRUDResourceAPI name c a i) m
crudResourceServer listAs getA postA updateA deleteA =
  listAs :<|> getA :<|> postA :<|> updateA :<|> deleteA
