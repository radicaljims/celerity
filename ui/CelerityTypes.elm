module CelerityTypes exposing (..)

import Time exposing (Time)

type alias Id = Int

type alias Copy =
  { kind : String
  , title : String
  , message : String
  , id : Id
  , started : Time
  }
