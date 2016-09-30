module EventTypes exposing (..)

import Json.Decode as Json exposing (..)
import Time exposing (Time)

type alias Id = Int

type alias Data =
  { timeStamp : String
  , eventType : String
  , filePath : String
  }

dataDecoder : Json.Decoder Data
dataDecoder =
  Json.object3
    Data
      ("timeStamp" := Json.string)
      ("eventType" := Json.string)
      ("filePath" := Json.string)

copyListDecoder : Json.Decoder (List Data)
copyListDecoder = Json.list dataDecoder
