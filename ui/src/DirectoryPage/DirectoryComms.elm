module DirectoryComms exposing (..)

type alias Fid = Int

-- import Http
-- import Task exposing (Task)

-- import DirectoryTypes exposing (Directory, directoryListDecoder)

-- fetchDirectories : (List Directory -> a) -> (String -> a) -> Cmd a
-- fetchDirectories s f =
--     Http.get directoryListDecoder "http://localhost:8081/directories"
--         |> Task.mapError toString
--         |> Task.perform f s
