module Utilities exposing (..)

import String exposing (contains)

lookup : List (String, a) -> String -> a -> a
lookup possibilities key default =
  case possibilities of
    p :: ps -> let (possibility, val) = p in
                if contains possibility key then
                  val
                else
                  lookup ps key default
    [] -> default

