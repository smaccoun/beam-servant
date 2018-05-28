{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Api.Resource where

import           AppPrelude
import           Pagination
import           Servant


{- Shorthand types for each CRUD endpoint -}
type GetListAPI c a =
  QueryParam "limit" Limit
    :> QueryParam "page" Offset
    :> QueryParam "orderBy" Order
    :> Get '[JSON] (c a)

type GetItemAPI a i =
  Capture "id" i    :> Get '[JSON] a

type CreateAPI baseEntity appEntity =
   ReqBody '[JSON] baseEntity :> Post '[JSON] appEntity

type DeleteAPI i =
   Capture "id" i :> Delete '[JSON] ()

type UpdateAPI a i =
   Capture "id" i :> ReqBody '[JSON] a :> Patch '[JSON] ()


{----- FULL CRUD ENDPOINTS -------}

type GetCollectionServer m c a = (Maybe Limit -> Maybe Offset -> Maybe Order -> m (c a))

{- Read API -}
type ReadOnlyEndpoints c a i =
       GetListAPI c a
      :<|> GetItemAPI a i

type RResourceAPI (resourceName :: Symbol) c a i =
  resourceName :> ReadOnlyEndpoints c a i

rResourceServer ::
     GetCollectionServer m c a
  -> (i -> m a)
  -> ServerT (RResourceAPI name c a i) m
rResourceServer listAs getA =
  listAs :<|> getA


{- Mutate Server. Commonly used under protected endpoints -}
type MutateEndpoints a baseEntity i =
         CreateAPI baseEntity a
    :<|> UpdateAPI baseEntity i
    :<|> DeleteAPI i

type CUDResourceAPI (resourceName :: Symbol) c a i baseEntity = resourceName :> MutateEndpoints a baseEntity i

cudResourceServer
  :: (baseEntity -> m a)
  -> (i -> baseEntity -> m ())
  -> (i -> m ())
  -> ServerT (CUDResourceAPI name c a i baseEntity) m
cudResourceServer postA updateA deleteA =
   postA :<|> updateA :<|> deleteA

{- Create/Read/Update/Delete API -}

type CRUDResourceAPI (resourceName :: Symbol) c a i baseEntity = resourceName :>
  (    GetListAPI c a
  :<|> GetItemAPI a i
  :<|> MutateEndpoints a baseEntity i
  )

type CRUDResourceServer name c a i baseEntity m =
     GetCollectionServer m c a
  -> (i -> m a)
  -> (baseEntity -> m a)
  -> (i -> baseEntity -> m ())
  -> (i -> m ())
  -> ServerT (CRUDResourceAPI name c a i baseEntity) m

crudResourceServer :: CRUDResourceServer name c a i baseEntity m
crudResourceServer listAs getA postA updateA deleteA =
  listAs :<|> getA :<|> postA :<|> updateA :<|> deleteA




{---- OTHER MISCELANEOUS CRUC COMOS ------}

{- Create/Read API -}
type CRResourceAPI (resourceName :: Symbol) c a i baseEntity = resourceName :>
  (    GetListAPI c a
  :<|> GetItemAPI a i
  :<|> CreateAPI baseEntity a
  )

crResourceServer
  :: GetCollectionServer m c a
  -> (i -> m a)
  -> (baseEntity -> m a)
  -> ServerT (CRResourceAPI name c a i baseEntity) m
crResourceServer listAs getA postA =
  listAs :<|> getA :<|> postA


{- Create/Read/Update API -}
type CRUResourceAPI (resourceName :: Symbol) c a i baseEntity = resourceName :>
  (    GetListAPI c a
  :<|> GetItemAPI a i
  :<|> CreateAPI baseEntity a
  :<|> UpdateAPI baseEntity i
  )

cruResourceServer
  :: GetCollectionServer m c a
  -> (i -> m a)
  -> (baseEntity -> m a)
  -> (i -> baseEntity -> m ())
  -> ServerT (CRUResourceAPI name c a i baseEntity) m
cruResourceServer listAs getA postA updateA =
  listAs :<|> getA :<|> postA :<|> updateA
