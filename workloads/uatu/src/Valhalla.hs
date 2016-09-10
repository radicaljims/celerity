module Valhalla (postEvent) where

import Control.Lens((^.))
import Network.Wreq (post, responseStatus, statusCode)

import Format

-- TODO: need to consider the possible error modes here (and everywhere else!)
postEvent event_type file_name = do
  r <- post "http://localhost:3000/fsevents" (jsonForEvent event_type file_name)
  print (r ^. responseStatus . statusCode)
