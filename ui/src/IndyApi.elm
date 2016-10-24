module IndyApi exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


type alias FSEvent =
  { eventType : String
  , filePath : String
  , timeStamp : String
  }

decodeFSEvent : Json.Decode.Decoder FSEvent
decodeFSEvent =
  Json.Decode.succeed FSEvent
    |: ("eventType" := Json.Decode.string)
    |: ("filePath" := Json.Decode.string)
    |: ("timeStamp" := Json.Decode.string)

getFsevents : Task.Task Http.Error (List (FSEvent))
getFsevents =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "fsevents"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeFSEvent)
      (Http.send Http.defaultSettings request)

type alias WatchedDirectory =
  { directoryPath : String
  , shortName : String
  , usedSpace : Int
  }

decodeWatchedDirectory : Json.Decode.Decoder WatchedDirectory
decodeWatchedDirectory =
  Json.Decode.succeed WatchedDirectory
    |: ("directoryPath" := Json.Decode.string)
    |: ("shortName" := Json.Decode.string)
    |: ("usedSpace" := Json.Decode.int)

getDirectories : Task.Task Http.Error (List (WatchedDirectory))
getDirectories =
  let
    request =
      { verb =
          "GET"
      -- TOOD: see if Chrome or someone is adding any headers for us in this case,
      -- as simply specifying a content-type seems to trip up CORS
      , headers = []
      --     [
      --     --   ("Origin", "http://localhost:8081")
      --     -- , ("Access-Control-Request-Method", "POST")
      --     -- , ("Access-Control-Request-Headers", "X-Custom-Header") ,
      --       ("Content-Type", "application/json")
      --     ]
      , url =
          "http://localhost:8081" ++
          "/" ++ "directories"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeWatchedDirectory)
      (Http.send Http.defaultSettings request)

type alias FileSystem =
  { files : List (String)
  }

decodeFileSystem : Json.Decode.Decoder FileSystem
decodeFileSystem =
  Json.Decode.succeed FileSystem
    |: ("files" := Json.Decode.list Json.Decode.string)

getStatusByDirectory : String -> Task.Task Http.Error (FileSystem)
getStatusByDirectory directory =
  let
    request =
      { verb =
          "GET"
      , headers =
          []
          -- [("Content-Type", "application/json")]
      , url =
          "http://localhost:8081" ++
          "/" ++ "status"
          ++ "/" ++ (directory |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeFileSystem
      (Http.send Http.defaultSettings request)

getHistoryByPath : String -> Task.Task Http.Error (List (FSEvent))
getHistoryByPath path =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "history"
          ++ "/" ++ (path |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeFSEvent)
      (Http.send Http.defaultSettings request)

type alias FileContent =
  { content : String
  }

decodeFileContent : Json.Decode.Decoder FileContent
decodeFileContent =
  Json.Decode.succeed FileContent
    |: ("content" := Json.Decode.string)

getContentByPath : String -> Task.Task Http.Error (FileContent)
getContentByPath path =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "content"
          ++ "/" ++ (path |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeFileContent
      (Http.send Http.defaultSettings request)
