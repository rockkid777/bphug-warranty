module Main where

import           Api.WarrantyService
import           Persistence.MySQL

main :: IO()
main = do
    handle <- makeHandle "db" "root" "warranties"
    run 80 handle
