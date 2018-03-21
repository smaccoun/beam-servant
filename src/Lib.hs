module Lib
    ( someFunc
    ) where

import AppPrelude
import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn ("Hello world!" :: T.Text)
