module Main where

import           App
import           Protolude

main :: IO ()
main = getArgs >>= startApp
