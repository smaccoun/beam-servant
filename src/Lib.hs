module Lib
    ( someFunc
    ) where

import qualified AppPrelude as AP
import qualified Data.Text as T

someFunc :: IO ()
someFunc = print "Hello world"
