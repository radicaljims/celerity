module Types exposing (..)

import Json.Decode as Json exposing (..)
import Time exposing (Time)

type alias Id = Int

type alias Directory =
  { directoryPath : String
  , shortName : String
  , usedSpace : Int }

directoryDecoder : Json.Decoder Directory
directoryDecoder =
  Json.object3
    Directory
      ("directoryPath" := Json.string)
      ("shortName" := Json.string)
      ("usedSpace" := Json.int)

directoryListDecoder : Json.Decoder (List Directory)
directoryListDecoder = Json.list directoryDecoder
