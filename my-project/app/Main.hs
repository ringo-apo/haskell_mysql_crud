{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Exception
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import qualified Data.Text.IO as T
import Data.Text

transactional :: MySQLConn -> IO a -> IO a
transactional conn procedure = mask $ \restore -> do
  execute_ conn "BEGIN"
  a <- restore procedure `onException` (execute_ conn "ROLLBACK")
  execute_ conn "COMMIT"
  pure a

main :: IO ()
main = do
    someFunc
