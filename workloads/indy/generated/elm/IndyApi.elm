module elm.IndyApi where

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

type alias Directory =
  { directoryPath : String
  , shortName : String
  , usedSpace : Int
  }

decodeDirectory : Json.Decode.Decoder Directory
decodeDirectory =
  Json.Decode.succeed Directory
    |: ("directoryPath" := Json.Decode.string)
    |: ("shortName" := Json.Decode.string)
    |: ("usedSpace" := Json.Decode.int)

getDirectories : Task.Task Http.Error (List (Directory))
getDirectories =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "directories"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeDirectory)
      (Http.send Http.defaultSettings request)

getStatusByDirectory : String -> Task.Task Http.Error (List (FSEvent))
getStatusByDirectory directory =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "status"
          ++ "/" ++ (directory |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeFSEvent)
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