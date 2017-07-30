{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Persistence.MySQL where

import           Data.UUID
import           Database.MySQL.Simple
import           Model.Warranty
import           Persistence.Handle
import           System.Random

makeHandle :: String -> String -> String -> IO (Handle IO)
makeHandle dbHost dbPass dbName = do
    conn <- connect $ defaultConnectInfo {
        connectHost = dbHost
    ,   connectPassword = dbPass
    ,   connectDatabase = dbName
    }

    return $ Handle {
        insert = insertSQL conn
    ,   update = updateSQL conn
    ,   list   = listSQL conn
    ,   get    = getSQL conn
    }

insertSQL :: Connection -> Warranty -> IO WarrantyId
insertSQL conn (Warranty exDate name price) = do
    uuid <- (randomIO :: IO UUID)
    query_ conn "insert into warranties values ('?', '?', '?', '?')"
        [uuid, exDate, name, price]
updateSQL :: Connection -> WarrantyId -> Warranty -> IO ()
updateSQL = undefined
listSQL   :: Connection -> IO [Warranty]
listSQL = undefined
getSQL    :: Connection -> WarrantyId -> IO Warranty
getSQL = undefined
