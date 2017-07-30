{-# LANGUAGE OverloadedStrings #-}
module Persistence.Handle where

import           Database.MySQL.Simple
import           Model.Warranty

data (Handle m) = Handle {
    insert :: Warranty -> m WarrantyId
,   update :: WarrantyId -> Warranty -> m ()
,   list   :: m [Warranty]
,   get    :: WarrantyId -> m Warranty
}
