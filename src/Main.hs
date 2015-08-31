{- |
Module      : Main
Description :
Copyright   : (c) Benoit Fraikin @ Solutions Lambda, 2015
License     : GPL-3
Maintainer  : solutions.lambda@gmail.com
Stability   : experimental
-}
module Main where

import           Data.RX 
import           Text.Parsec.ByteString.Lazy (parseFromFile)

-- | The main entry point.
main :: IO ()
main = testParser

testParser = do result <- parseFromFile rx "test.txt"
                case result of
                    Left msg -> print msg 
                    Right x -> print x

