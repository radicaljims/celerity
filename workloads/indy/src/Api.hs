{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Api (API, fsEventsAPI, indySwagger, indyAPI, IndyAPI) where

import Control.Lens
import Data.Swagger
import Servant
import Servant.Swagger
import Types

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type IndyAPI = "fsevents" :> Get '[JSON] [FSEvent]
  :<|> "directories" :> Get '[JSON] [WatchedDirectory]
  :<|> "status" :> Capture "directory" String :> Get '[JSON] FileSystem
  :<|> "history" :> Capture "path" String :> Get '[JSON] [FSEvent]
  :<|> "content" :> Capture "path" String :> Get '[JSON] FileContent

indyAPI :: Proxy IndyAPI
indyAPI = Proxy

-- note: for what I can only consider is no good reason,
-- it seems like SwaggerAPI has to come first
type API =
  SwaggerAPI :<|> IndyAPI

fsEventsAPI :: Proxy API
fsEventsAPI = Proxy

indySwagger :: Swagger
indySwagger = toSwagger indyAPI
  & info.title .~ "Indy API"
  & info.version .~ "0.1"
  & info.description ?~ "Indirecting access to Valhalla"
