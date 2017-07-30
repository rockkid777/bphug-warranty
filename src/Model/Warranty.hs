{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Model.Warranty where

import           Data.Aeson
import           Data.Text     (Text)
import           Data.Time     (UTCTime (..), fromGregorian)
import           Data.Typeable (Typeable)
import           GHC.Generics
import           Servant       (FromHttpApiData)

data Warranty = Warranty
  { expiryDate :: UTCTime  -- ^ Expiry datetime.
  , name       :: Text     -- ^ Item name.
  , price      :: Int      -- ^ Price of thte item
  } deriving (Show, Generic, Typeable)

newtype WarrantyId = WarrantyId Text
    deriving (Show, Generic, Typeable, ToJSON, FromHttpApiData)
