import Html.App as App
import Model exposing (init, update, Mdl)
import View  exposing (view)

import Material

main : Program Never
main =
  App.program
      { init = init
      , update = update
      , view = view
      , subscriptions = \model -> Material.subscriptions Mdl model
      }

-- testString : String
-- -- testString = "[{\"id\" : 1, \"data\" : { \"eventType\" : \"addormod\", \"filePath\" : \"/hi.there\"}}]"
-- testString = "[{\"timeStamp\":\"time stamp!\", \"eventType\":\"addormod\", \"filePath\": \"/hi.there\"}]"

-- fuddyDuddy maybeCopies =
--     case maybeCopies of
--         Ok copies -> copies
--         Err no_copies -> []

-- main : Html Comms
-- main = div [] (List.map renderCopy (fuddyDuddy (Json.decodeString copyListDecoder testString)))
