module Server(app1, mockServer) where

import Servant
import Servant.Mock

import Api
import Types

server1 :: Server API
server1 = return fsevents1

app1 :: Application
app1 = serve fsEventsAPI server1

mockServer :: Application
mockServer = serve fsEventsAPI $ mock fsEventsAPI Proxy