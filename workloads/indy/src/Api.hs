{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api (API, fsEventsAPI) where

import Servant
import Types

type API = "fsevents" :> Get '[JSON] [FSEvent]

fsEventsAPI :: Proxy API
fsEventsAPI = Proxy