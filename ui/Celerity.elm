import Bootstrap.Html exposing (..)
import Html exposing (..)
import Html.Attributes as A exposing (style, class)
import Html.App as App
import Html.Events exposing (..)
import Html.Shorthand exposing (..)
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
    { copies : List Data
    , message : String
    }

emptyModel : Model
emptyModel =  { copies = [], message = "" }

init : (Model, Cmd a)
init = emptyModel ! []

type Comms =  NoOp | Get | GetSuccess (List Data) | GetFailure String

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
    Http.get copyListDecoder "http://localhost:8081/fsevents"
        |> Task.mapError toString
        |> Task.perform GetFailure GetSuccess

dataDecoder : Json.Decoder Data
dataDecoder =
    Json.object3
        Data
            ("timeStamp" := Json.string)
            ("eventType" := Json.string)
            ("filePath" := Json.string)

copyListDecoder : Json.Decoder (List Data)
copyListDecoder = Json.list dataDecoder

prettyTypes : List (String, String)
prettyTypes = [("addormod", "Added or Modified"), ("delete", "Deleted")]

styleTypes : List (String, Styles.Styles)
styleTypes = [("addormod", Styles.addormod), ("delete", Styles.delete)]

classTypes : List (String, String)
classTypes = [("addormod", "success"), ("delete", "danger")]

lookup : List (String, a) -> String -> a -> a
lookup possibilities key default =
    case possibilities of
        p :: ps -> let (possibility, val) = p in
                    if contains possibility key then
                        val
                    else
                        lookup ps key default
        [] -> default

renderCopy : Data -> Html Comms
renderCopy copy =
    let eventStyle = (lookup styleTypes copy.eventType Styles.plainEvent)
    in
    tr_
      [ td [style eventStyle] [text (lookup prettyTypes copy.eventType copy.eventType)]
      , td  [(style Styles.filePath)] [text copy.filePath]]

view : Model -> Html Comms
view model =
    container_
        [ Styles.stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
        , row_ [header]
        , row_ [fetch model]
        , row_ [(body model)]]

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

fetch : Model -> Html Comms
fetch model =
    container_
        [ Styles.stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
        , row_
            (append
                 [(colMd_ 1 1 1 [makeButton Get "Get Events"])]
                 [(colMd_ 0 0 0 [text model.message])])
                 -- [(colXs_ 0 [makeButton Get "Get Events"])]
                 -- [(colXs_ 0 [text model.message])])
                 -- [colXsOffset_ 0 1 [text model.message]])
        ]

body : Model -> Html Comms
body model =
    table
        [ A.class "table col-xs-10 table-hover table-striped" ]
        [ thead_
              [ tr_
                    [ th [A.class "col-xs-2"] [text "Event"]
                    , th_ [text "File"]
                    ]
              ]
              , tbody_ (List.map renderCopy model.copies)
        ]

-- testString : String
-- -- testString = "[{\"id\" : 1, \"data\" : { \"eventType\" : \"addormod\", \"filePath\" : \"/hi.there\"}}]"
-- testString = "[{\"timeStamp\":\"time stamp!\", \"eventType\":\"addormod\", \"filePath\": \"/hi.there\"}]"

-- fuddyDuddy maybeCopies =
--     case maybeCopies of
--         Ok copies -> copies
--         Err no_copies -> []

-- main : Html Comms
-- main = div [] (List.map renderCopy (fuddyDuddy (Json.decodeString copyListDecoder testString)))
