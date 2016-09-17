module Server(app1, mockServer) where

import Network.Wai.Middleware.Cors
import Servant
import Servant.Mock

import Api
import Types

server1 :: Server API
server1 = (return fsevents1) :<|> (return directory1) :<|> status :<|> history :<|> content
  where status :: String -> Handler [FSEvent]
        status _ = return fsevents2

        history :: String -> Handler [FSEvent]
        history _ = return fsevents2

        content :: String -> Handler FileContent
        content _ = return content1

app1 :: Application
app1 = serve fsEventsAPI server1

mockServer :: Application
mockServer = simpleCors (serve fsEventsAPI $ mock fsEventsAPI Proxy)
