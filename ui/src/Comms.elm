module Comms exposing (..)

import Http
import Types exposing (copyListDecoder, Data, Directory, directoryListDecoder)
import Task exposing (Task)

fetchCopies : (List Data -> a) -> (String -> a) -> Cmd a
fetchCopies s f =
    Http.get copyListDecoder "http://localhost:8081/fsevents"
        |> Task.mapError toString
        |> Task.perform f s

fetchDirectories : (List Directory -> a) -> (String -> a) -> Cmd a
fetchDirectories s f =
    Http.get directoryListDecoder "http://localhost:8081/directories"
        |> Task.mapError toString
        |> Task.perform f s
