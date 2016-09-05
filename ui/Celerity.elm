import Html exposing (..)
import Html.Attributes exposing (style)
import Html.App as App
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (..)
import List exposing (..)
import String exposing (contains)
import Task exposing (Task)

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

prettyTypes : List (String, String)
prettyTypes = [("addormod", "Added or Modified"), ("delete", "Deleted")]

styleTypes : List (String, Styles.Styles)
styleTypes = [("addormod", Styles.addormod), ("delete", Styles.delete)]

lookup : List (String, a) -> String -> a -> a
lookup possibilities key default =
    case possibilities of
        p :: ps -> let (possibility, val) = p in
                    if contains possibility key then
                        val
                    else
                        lookup ps key default
        [] -> default

renderCopy : Copy -> Html Comms
renderCopy copy =
  div
    [ style (lookup styleTypes copy.data.eventType Styles.plainEvent) ]
    [ span
        []
        [ span [] [text (lookup prettyTypes copy.data.eventType copy.data.eventType)]
        , br [] []
        , span [style Styles.fileName]  [text copy.data.fileName]]
    ]

view : Model -> Html Comms
view model =
  div
    [style Styles.container]
    [header, fetch, (body model)]

makeButton : Comms -> String -> Html Comms
makeButton action btn_text =
  button
    [onClick action]
    [text btn_text]

header : Html Comms
header =
  div
    [style Styles.header]
    [ h1 [] [text "Celerity"]]

fetch : Html Comms
fetch =
    div
        [style Styles.center]
        [makeButton Get "Get Events"]

body : Model -> Html Comms
body model =
  div
    [style Styles.body]
    (List.map renderCopy model.copies)

-- testString : String
-- testString = "[{\"id\" : 1, \"data\" : { \"eventType\" : \"addormod\", \"fileName\" : \"/hi.there\"}}]"

-- fuddyDuddy maybeCopies =
--     case maybeCopies of
--         Ok copies -> copies
--         Err no_copies -> []

-- main : Html Comms
-- main = div [] (List.map renderCopy (fuddyDuddy (Json.decodeString copyListDecoder testString)))
