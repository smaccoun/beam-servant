{-# LANGUAGE DeriveAnyClass #-}

module Pagination where

import           AppPrelude
import           Data.Aeson      (FromJSON, ToJSON, Value (..), object,
                                  parseJSON, toJSON, (.:), (.=))
import           Data.Time.Clock

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
         DefaultLimit    -> defaultLimit
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
