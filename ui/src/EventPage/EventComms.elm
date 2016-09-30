module EventComms exposing (..)

import Http
import Task exposing (Task)

import EventTypes exposing (copyListDecoder, Data)

fetchCopies : (List Data -> a) -> (String -> a) -> Cmd a
fetchCopies s f =
    Http.get copyListDecoder "http://localhost:8081/fsevents"
        |> Task.mapError toString
        |> Task.perform f s
