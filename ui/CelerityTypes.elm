module CelerityTypes exposing (..)

import Time exposing (Time)

type alias Id = Int

type alias Data =
    { eventType : String
    , fileName : String
    }

type alias Copy =
  { id : Id
  , data : Data
  }
