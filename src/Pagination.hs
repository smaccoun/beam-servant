{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Pagination where

import           Api.Resource
import           AppPrelude
import           Data.Aeson                   (FromJSON, ToJSON, Value (..),
                                               object, parseJSON, toJSON, (.:),
                                               (.=))
import           Database.Beam.Backend.SQL
import           Database.Beam.Query
import           Database.Beam.Query.Internal
import           Servant

newtype Limit = Limit Integer
newtype Offset = Offset Integer

data Pagination =
  Pagination
    { pageLimit  :: Limit
    , pageOffset :: Offset
    }

data LimitArg = DefaultLimit | CustomLimit Limit

pagination :: LimitArg -> Offset -> Pagination
pagination limitArg offset =
  Pagination
    {pageLimit =
       case limitArg of
         DefaultLimit       -> defaultLimit
         CustomLimit limit' -> limit'
    ,pageOffset = offset
    }

data PaginatedResult entity = PaginatedResult
  { __pagination :: PaginationContext
  , __data       :: [entity]
  }

data PaginationContext = PaginationContext
  { currentPage  :: Integer
  , previousPage :: Maybe Integer
  , nextPage     :: Maybe Integer
  , totalPages   :: Integer
  , count        :: Integer
  , perPage      :: Integer
  }
  deriving (Generic, ToJSON, FromJSON)

instance Functor PaginatedResult where
  fmap f c = c { __data = f <$> __data c }

instance (ToJSON entity) => ToJSON (PaginatedResult entity) where
  toJSON collection =
    object [ "pagination" .= __pagination collection
           , "data" .= __data collection
           ]

instance (FromJSON entity) => FromJSON (PaginatedResult entity) where
  parseJSON (Object c) = PaginatedResult
    <$> c .: "pagination"
    <*> c .: "data"

  parseJSON _ = mzero

defaultLimit :: Limit
defaultLimit = Limit 10

defaultOffset :: Offset
defaultOffset = Offset 0


paginateQuery :: (Sql92ProjectionExpressionSyntax
      (Sql92SelectTableProjectionSyntax
          (Sql92SelectSelectTableSyntax syntax))
    ~
    Sql92SelectTableExpressionSyntax
      (Sql92SelectSelectTableSyntax syntax),
    Sql92TableSourceSelectSyntax
      (Sql92FromTableSourceSyntax
          (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax syntax)))
    ~
    syntax,
    ThreadRewritable
      (QNested QueryInaccessible)
      (WithRewrittenThread
          (QNested (QNested QueryInaccessible))
          (QNested QueryInaccessible)
          a),
    ThreadRewritable (QNested (QNested QueryInaccessible)) a,
    ProjectibleWithPredicate
      ValueContext
      (Sql92ProjectionExpressionSyntax
          (Sql92SelectTableProjectionSyntax
            (Sql92SelectSelectTableSyntax syntax)))
      a,
    ProjectibleWithPredicate
      ValueContext
      (Sql92ProjectionExpressionSyntax
          (Sql92SelectTableProjectionSyntax
            (Sql92SelectSelectTableSyntax syntax)))
      (WithRewrittenThread
          (QNested (QNested QueryInaccessible))
          (QNested QueryInaccessible)
          a),
    ProjectibleWithPredicate
      ValueContext
      (Sql92ProjectionExpressionSyntax
          (Sql92SelectTableProjectionSyntax
            (Sql92SelectSelectTableSyntax syntax)))
      (WithRewrittenThread
          (QNested QueryInaccessible)
          QueryInaccessible
          (WithRewrittenThread
            (QNested (QNested QueryInaccessible))
            (QNested QueryInaccessible)
            a)),
    ProjectibleWithPredicate
      AnyType
      (Sql92ProjectionExpressionSyntax
          (Sql92SelectTableProjectionSyntax
            (Sql92SelectSelectTableSyntax syntax)))
      a,
    ProjectibleWithPredicate
      AnyType
      (Sql92ProjectionExpressionSyntax
          (Sql92SelectTableProjectionSyntax
            (Sql92SelectSelectTableSyntax syntax)))
      (WithRewrittenThread
          (QNested (QNested QueryInaccessible))
          (QNested QueryInaccessible)
          a),
    ProjectibleWithPredicate
      AnyType
      (Sql92ProjectionExpressionSyntax
          (Sql92SelectTableProjectionSyntax
            (Sql92SelectSelectTableSyntax syntax)))
      (WithRewrittenThread
          (QNested QueryInaccessible)
          QueryInaccessible
          (WithRewrittenThread
            (QNested (QNested QueryInaccessible))
            (QNested QueryInaccessible)
            a)),
    HasQBuilder syntax) =>
    Pagination
    -> Q syntax db (QNested (QNested QueryInaccessible)) a
    -> SqlSelect
        syntax
        (QExprToIdentity
            (WithRewrittenThread
              (QNested QueryInaccessible)
              QueryInaccessible
              (WithRewrittenThread
                  (QNested (QNested QueryInaccessible))
                  (QNested QueryInaccessible)
                  a)))
paginateQuery (Pagination (Limit limit') (Offset offset')) query' =
  select $ limit_ limit' $ offset_ offset' $ query'

newtype TotalCount = TotalCount Int

getPaginationContext :: Pagination -> TotalCount -> PaginationContext
getPaginationContext (Pagination (Limit perPage) (Offset currentPage) ) (TotalCount count) =
  PaginationContext
    {count = count'
    ,perPage = perPage
    ,currentPage = currentPage
    ,previousPage =
          if currentPage <= 0
          then Nothing
          else (Just $ currentPage - 1)
    , nextPage =
          if (currentPage + 1) * perPage < count'
          then (Just $ currentPage + 1)
          else Nothing
    , totalPages = ceiling $ (fromIntegral count / fromIntegral perPage :: Double)
    }
  where
    count' = fromIntegral count


type PaginatedGetAPI getReturn =
       (  QueryParam "limit" Int
       :> QueryParam "page" Int
       :> QueryParam "orderBy" Text
       :> Get '[JSON] getReturn
       )


type RPaginatedResultAPI (resourceName :: Symbol) a i =
  RResourceAPI resourceName PaginatedResult a i


