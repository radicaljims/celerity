{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Api
import           Data.Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Types

main :: IO ()
main = BL8.writeFile "generated/swagger/swagger.json" (encodePretty indySwagger)
