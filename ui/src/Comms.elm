module Comms exposing (..)

import Http
import Types exposing (copyListDecoder, Data)
import Task exposing (Task)

fetchCopies : (List Data -> a) -> (String -> a) -> Cmd a
fetchCopies s f =
    Http.get copyListDecoder "http://localhost:8081/fsevents"
        |> Task.mapError toString
        |> Task.perform f s

