{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Api.WarrantyService where

import           Control.Lens
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.Proxy
import           Data.Swagger
import           Data.Text                  (Text)
import           Data.Text.Encoding
import           Data.Time                  (UTCTime (..), fromGregorian)
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Swagger
import           System.IO

warrantyAPI :: Proxy API
warrantyAPI = Proxy

-- | A single Todo entry.
data Warranty = Warranty
  { expiryDate :: UTCTime  -- ^ Expiry datetime.
  , name       :: Text     -- ^ Item name.
  , price      :: Int      -- ^ Price of thte item
  } deriving (Show, Generic, Typeable)

-- | A unique Warranty entry ID.
newtype WarrantyId = WarrantyId Text
    deriving (Show, Generic, Typeable, ToJSON, FromHttpApiData)

-- | The API of a Warranty service.
type WarrantyAPI
    = "warranty" :> Get '[JSON] [Warranty]
    :<|> "warranty" :> Capture "id" WarrantyId :> Get '[JSON] Warranty
    :<|> "warranty" :> Capture "id" WarrantyId :> ReqBody '[JSON] Warranty
            :> Put '[JSON] WarrantyId
    :<|> "warranty" :> ReqBody '[JSON] Warranty :> Post '[JSON] WarrantyId

-- | API for serving @swagger.json@.
type SwaggerAPI = "swagger.json" :> Get '[JSON] String

-- | Combined API of a Warranty service with Swagger documentation.
type API = SwaggerAPI :<|> WarrantyAPI

instance ToJSON Warranty
instance FromJSON Warranty

instance ToSchema Warranty where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "This is some real Warranty right here"
    & mapped.schema.example ?~ toJSON (Warranty (UTCTime (fromGregorian 2018 12 31) 0) "Tv" 200000)

instance ToParamSchema WarrantyId
instance ToSchema WarrantyId

-- | Swagger spec for Warranty API.
warrantySwagger :: String
warrantySwagger = show $ toSwagger warrantyAPI
  & info.title   .~ "Warranty API"
  & info.version .~ "0.1"
  & info.description ?~ "This is an API that gives details about Warranty service"

serveListWarranties = return []
serveGetWarranty _ = return $ Warranty (UTCTime (fromGregorian 2018 12 31) 0) "Tv" 200000
servePutWarranty id' _ = return id'
servePostWarranty _ = return $ WarrantyId "777"

-- | Combined server of a Warranty service with Swagger documentation.
server :: Server API
server = return warrantySwagger :<|> (serveListWarranties :<|> serveGetWarranty :<|>
    servePutWarranty :<|> servePostWarranty)

-- -- | Output generated @swagger.json@ file for the @'WarrantyAPI'@.
-- writeSwaggerJSON :: IO ()
-- writeSwaggerJSON = BL8.writeFile "example/swagger.json" (encodePretty warrantySwagger)

mkApp :: IO Application
mkApp = return $ serve warrantyAPI server

run :: Int -> IO ()
run port = do
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp
