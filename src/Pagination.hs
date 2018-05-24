{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Pagination where

import           AppPrelude
import           Data.Aeson                   (FromJSON, ToJSON, Value (..),
                                               object, parseJSON, toJSON, (.:),
                                               (.=))
import           Data.Time.Clock
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

data Collection entity = Collection
  { __metadata   :: CollectionMetadata
  , __pagination :: PaginationResult
  , __data       :: [entity]
  }

data CollectionMetadata = CollectionMetadata
  { lastUpdatedAt    :: Maybe UTCTime
  }
  deriving (Generic, ToJSON, FromJSON)

data PaginationResult = PaginationResult
  { currentPage  :: Int
  , previousPage :: Maybe Int
  , nextPage     :: Maybe Int
  , totalPages   :: Int
  , count        :: Int
  , perPage      :: Int
  }
  deriving (Generic, ToJSON, FromJSON)

instance Functor Collection where
  fmap f c = c { __data = f <$> __data c }

instance (ToJSON entity) => ToJSON (Collection entity) where
  toJSON collection =
    object [ "metadata" .= __metadata collection
           , "pagination" .= __pagination collection
           , "data" .= __data collection
           ]

instance (FromJSON entity) => FromJSON (Collection entity) where
  parseJSON (Object c) = Collection
    <$> c .: "metadata"
    <*> c .: "pagination"
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


type PaginatedGetAPI getReturn =
       (  QueryParam "limit" Int
       :> QueryParam "page" Int
       :> QueryParam "orderBy" Text
       :> Get '[JSON] getReturn
       )
