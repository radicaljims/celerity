module View exposing (..)

import Types exposing (Data)

import Bootstrap.Html exposing (..)
import Html exposing (..)
import Html.Attributes as A exposing (style, class)
import Html.Shorthand exposing (..)

import Material.Progress as Progress
import Material.Scheme
import Material.Button as Button
import Material.Options exposing (css)

import List exposing (..)

import Model exposing (Msg, Model, Mdl)
import Styles
import Utilities exposing (lookup)

prettyTypes : List (String, String)
prettyTypes = [("addormod", "Added or Modified"), ("delete", "Deleted")]

styleTypes : List (String, Styles.Styles)
styleTypes = [("addormod", Styles.addormod), ("delete", Styles.delete)]

classTypes : List (String, String)
classTypes = [("addormod", "success"), ("delete", "danger")]

view : Model -> Html Msg
view model =
    container_
        [ Styles.stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
        , row_ [header]
        , row_ [fetch model]
        , row_ [(body model)]]

header : Html Msg
header =
  div
    [style Styles.header]
    [ h1 [] [text "Celerity"]]

fetch : Model -> Html Msg
fetch model =
    container_
        [ Styles.stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
        , makeBusyButton model model.fetching 0 Model.Get "Get Events"
        ]

    |> Material.Scheme.top

body : Model -> Html Msg
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

makeButton : Model -> Int -> Msg -> String -> Html Msg
makeButton model idx action btnText =
  Button.render Mdl [idx] model.mdl
    [ Button.raised, Button.colored, Button.onClick action, css "margin" "0 10px 0 10px"]
    [ text btnText ]

makeBusyButton : Model -> Bool -> Int -> Msg -> String -> Html Msg
makeBusyButton model isBusy idx action btnText =
  row_
    (append
      [makeButton model idx action btnText]
      (if isBusy then [ Progress.indeterminate ] else [])
    )

renderCopy : Data -> Html Msg
renderCopy copy =
  let eventStyle = (lookup styleTypes copy.eventType Styles.plainEvent)
  in
  tr_
    [ td [style eventStyle] [text (lookup prettyTypes copy.eventType copy.eventType)]
    , td  [(style Styles.filePath)] [text copy.filePath]]
