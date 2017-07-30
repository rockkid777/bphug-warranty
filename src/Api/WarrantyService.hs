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
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Proxy
import           Data.Swagger
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime (..), fromGregorian)
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Swagger
import           System.IO

todoAPI :: Proxy API
todoAPI = Proxy

-- | The API of a Todo service.
type TodoAPI
    = "todo" :> Get '[JSON] [Todo]
 :<|> "todo" :> ReqBody '[JSON] Todo :> Post '[JSON] TodoId
 :<|> "todo" :> Capture "id" TodoId :> Get '[JSON] Todo
 :<|> "todo" :> Capture "id" TodoId :> ReqBody '[JSON] Todo :> Put '[JSON] TodoId

-- | API for serving @swagger.json@.
type SwaggerAPI = "swagger.json" :> Get '[JSON] String

-- | Combined API of a Todo service with Swagger documentation.
type API = SwaggerAPI :<|> TodoAPI

-- | A single Todo entry.
data Todo = Todo
  { created :: UTCTime  -- ^ Creation datetime.
  , summary :: Text     -- ^ Task summary.
  } deriving (Show, Generic, Typeable)

-- | A unique Todo entry ID.
newtype TodoId = TodoId Int
  deriving (Show, Generic, Typeable, ToJSON, FromHttpApiData)

instance ToJSON Todo
instance FromJSON Todo

instance ToSchema Todo where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "This is some real Todo right here"
    & mapped.schema.example ?~ toJSON (Todo (UTCTime (fromGregorian 2015 12 31) 0) "get milk")

instance ToParamSchema TodoId
instance ToSchema TodoId

-- | Swagger spec for Todo API.
todoSwagger :: String
todoSwagger = show $ toSwagger todoAPI
  & info.title   .~ "Todo API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

-- | Combined server of a Todo service with Swagger documentation.
server :: Server API
server = return todoSwagger :<|> error "not implemented"

-- | Output generated @swagger.json@ file for the @'TodoAPI'@.
writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "example/swagger.json" (encodePretty todoSwagger)

mkApp :: IO Application
mkApp = return $ serve todoAPI server

run :: Int -> IO ()
run port = do
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp
