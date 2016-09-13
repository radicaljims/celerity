module CelerityTypes exposing (..)

import Time exposing (Time)

type alias Id = Int

type alias Data =
    { timeStamp : String
    , eventType : String
    , filePath : String
    }

-- type alias Copy =
--   { id : Id
--   , data : Data
--   }
