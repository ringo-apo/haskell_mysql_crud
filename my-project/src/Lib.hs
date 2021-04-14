{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Control.Exception
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.Text
import qualified Data.Text.IO as T
import Data.Text.Internal (Text(..), safe, text)

transactional :: MySQLConn -> IO a -> IO a
transactional conn procedure = mask $ \restore -> do
    execute_ conn "BEGIN"
    a <- restore procedure `onException` (execute_ conn "ROLLBACK")
    execute_ conn "COMMIT"
    pure a

someFunc :: IO ()
someFunc = do
    print "Please input menu key."
    print "1:Select 2:Insert 3:Delete 4:Update 5:End"

    menun <- getLine

    case menun of
        "1" -> do
            print "id name comment time"
            conn <- connect
                defaultConnectInfo {ciUser = "root", ciPassword = "Password1234!", ciDatabase = "test"}
            (defs, is) <- query_ conn "SELECT * FROM memos"

            let f :: MySQLValue -> IO ()

                f (MySQLText text) = do
                    T.putStr text
                    T.putStr " "

                f (MySQLInt32 int) = do
                    T.putStr (Data.Text.pack (show int))
                    T.putStr " "

                f (MySQLDateTime text) = do
                    T.putStr (Data.Text.pack (show text))
                    T.putStrLn ""

                f _other = return ()

                in mapM_ (mapM f) =<< Streams.toList is

            someFunc            

        "2" -> do
            -- let comment = "apple"
            print "Input name"
            name <- getLine
            let nameb = "\"" ++ name ++ "\""
            let namec = read nameb :: Data.Text.Internal.Text

            print "Input comment"
            a <- getLine
            let b = "\"" ++ a ++ "\""
            let comment = read b :: Data.Text.Internal.Text
            let id = 3
            conn <- connect
                defaultConnectInfo {ciUser = "root", ciPassword = "Password1234!", ciDatabase = "test"}
            stmt <- prepareStmt conn "INSERT INTO memos (name, comment, time) VALUES (?, ?, NOW())"
            transactional conn $ do
                executeStmt conn stmt [MySQLText namec, MySQLText comment]
                (defs, is) <- query_ conn "SELECT * FROM memos"
                mapM_ print =<< Streams.toList is

            someFunc 

        "3" -> do
            print "Input id number"
            id <- getLine
            let idb = "\"" ++ id ++ "\""
            let idc = read idb :: Data.Text.Internal.Text

            conn <- connect
                defaultConnectInfo {ciUser = "root", ciPassword = "Password1234!", ciDatabase = "test"}
            stmt <- prepareStmt conn "delete from memos where id=?"
            transactional conn $ do
                executeStmt conn stmt [MySQLText idc]

                (defs, is) <- query_ conn "SELECT * FROM memos"
                mapM_ print =<< Streams.toList is

            someFunc

        "4" -> do
            print "Input id number"
            id <- getLine
            let idb = "\"" ++ id ++ "\""
            let idc = read idb :: Data.Text.Internal.Text

            print "Input comment"
            comment <- getLine
            let commentb = "\"" ++ comment ++ "\""
            let commentc = read commentb :: Data.Text.Internal.Text

            conn <- connect
                defaultConnectInfo {ciUser = "root", ciPassword = "Password1234!", ciDatabase = "test"}
            stmt <- prepareStmt conn "update memos set comment=? , time=NOW() where id=?"
            transactional conn $ do
                executeStmt conn stmt [MySQLText commentc, MySQLText idc]
                (defs, is) <- query_ conn "SELECT * FROM memos"
                mapM_ print =<< Streams.toList is
            someFunc

        _ -> do
          print "End"

