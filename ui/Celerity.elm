import Html exposing (..)
import Html.Attributes exposing (style)
import Html.App as App
import Html.Events exposing (..)
import Http
import Task exposing (Task)

import Json.Decode as Json exposing (..)

import List exposing (..)

import CelerityTypes exposing (..)

import Styles

main : Program Never
main = App.program
       { init = init
       , update = update
       , view = view
       , subscriptions = \_ -> Sub.none
       }

type alias Model =
    { copies : List Copy
    , message : String
    }

emptyModel : Model
emptyModel =  { copies = [], message = "" }

init = emptyModel ! []

type Comms =  NoOp | Get | GetSuccess (List Copy) | GetFailure String

update : Comms -> Model -> (Model, Cmd Comms)
update comm model =
  case comm of
      NoOp -> model ! []

      Get ->
          { model | message = "Fetching copies..." } ! [fetchCopies]

      GetSuccess copies ->
          { model | message = "Fetched copies", copies = copies} ! []

      GetFailure error ->
          { model | message = error } ! []

fetchCopies : Cmd Comms
fetchCopies =
    Http.get copyListDecoder "http://localhost:3000/fsevents"
        |> Task.mapError toString
        |> Task.perform GetFailure GetSuccess


dataDecoder : Json.Decoder Data
dataDecoder =
    Json.object2
        Data
            ("eventType" := Json.string)
            ("fileName" := Json.string)

copyDecoder : Json.Decoder Copy
copyDecoder =
    Json.object2
        Copy
            ("id" := Json.int)
            ("data" := dataDecoder)

copyListDecoder : Json.Decoder (List Copy)
copyListDecoder = Json.list copyDecoder

renderCopy : Copy -> Html Comms
renderCopy copy =
  div
    [ style Styles.copy ]
    [ span
        []
        [text (copy.data.eventType ++ " : " ++ copy.data.fileName)]
    ]

view : Model -> Html Comms
view model =
  div
    []
    (header :: (List.map renderCopy model.copies))

makeButton : Comms -> String -> Html Comms
makeButton action btn_text =
  button
    [onClick action]
    [text btn_text]

header : Html Comms
header =
  div
    [ style Styles.center]
    [ (makeButton Get "Get Copies")]

-- testString : String
-- testString = "[{\"id\" : 1, \"data\" : { \"eventType\" : \"addormod\", \"fileName\" : \"/hi.there\"}}]"

-- fuddyDuddy maybeCopies =
--     case maybeCopies of
--         Ok copies -> copies
--         Err no_copies -> []

-- main : Html Comms
-- main = div [] (List.map renderCopy (fuddyDuddy (Json.decodeString copyListDecoder testString)))
