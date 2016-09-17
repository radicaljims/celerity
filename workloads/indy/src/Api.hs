{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Api (API, fsEventsAPI) where

import Servant
import Types

type API = "fsevents" :> Get '[JSON] [FSEvent]
  :<|> "directories" :> Get '[JSON] [Directory]
  :<|> "status" :> Capture "directory" String :> Get '[JSON] [FSEvent]
  :<|> "history" :> Capture "path" String :> Get '[JSON] [FSEvent]
  :<|> "content" :> Capture "path" String :> Get '[JSON] FileContent

fsEventsAPI :: Proxy API
fsEventsAPI = Proxy
