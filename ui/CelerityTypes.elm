module ToastTypes exposing (..)

import Time exposing (Time)

type alias Id = Int

type alias Toast =
  { kind : String
  , title : String
  , message : String
  , id : Id
  , started : Time
  }
