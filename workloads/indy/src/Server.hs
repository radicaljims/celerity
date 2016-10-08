module Server(server1, indy, mockServer, indyServer) where

import Network.Wai.Middleware.Cors
import Servant
import Servant.Mock

import Api
import Types

-- indyServer doesn't contain the swagger stuff
indyServer :: Server IndyAPI
indyServer = (return fsevents1) :<|> (return directory1) :<|> status :<|> history :<|> content
  where status :: String -> Handler [FSEvent]
        status _ = return fsevents2

        history :: String -> Handler [FSEvent]
        history _ = return fsevents2

        content :: String -> Handler FileContent
        content _ = return content1

-- LOOK WE CAN LITERALLY ADD SERVERS TOGETHER COME ON GUYS THAT'S COOL
server1 :: Server API
server1 = (return indySwagger) :<|> indyServer

indy :: Application
indy = serve (Proxy :: Proxy API) server1

mockServer :: Application
mockServer = simpleCors (serve indyAPI $ mock indyAPI Proxy)
