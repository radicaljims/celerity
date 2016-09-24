module Server(indy, mockServer) where

import Network.Wai.Middleware.Cors
import Servant
import Servant.Mock

import Api
import Types

server1 :: Server API
server1 = (return indySwagger) :<|> (return fsevents1) :<|> (return directory1) :<|> status :<|> history :<|> content
  where status :: String -> Handler [FSEvent]
        status _ = return fsevents2

        history :: String -> Handler [FSEvent]
        history _ = return fsevents2

        content :: String -> Handler FileContent
        content _ = return content1

indy :: Application
indy = serve (Proxy :: Proxy API) server1

mockServer :: Application
mockServer = simpleCors (serve indyAPI $ mock indyAPI Proxy)
-- mockServer = simpleCors (serve (return indyAPI) $ mock (return indyAPI) Proxy)
