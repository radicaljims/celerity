module View exposing (..)

import Types exposing (Data)

import Bootstrap.Html exposing (..)
import Html exposing (..)
import Html.Attributes as A exposing (style, class)
-- import Html.Shorthand exposing (..)

import Material.List as Lists
-- import Material.Icon as Icon
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

iconNames : List (String, String)
iconNames = [("addormod", "note_add"), ("delete", "delete")]

view : Model -> Html Msg
view model =
  container_
      [ Styles.stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
      , row_ [header]
      , row_ [fetch model]
      , row_ [body model]]

  |> Material.Scheme.top

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

body : Model -> Html Msg
body model =
  Lists.ul
    [ css "margin" "0", css "padding" "0"]
    (List.map listItem model.copies)

listItem : Data -> Html Msg
listItem data =
  let prettyType = lookup prettyTypes data.eventType data.eventType
      iconName = lookup iconNames data.eventType "motorcycle"
  in
    Lists.li
      [ Lists.withSubtitle ]
      [ Lists.content
          []
          [ Lists.icon
              iconName []
          ,   text prettyType
          ,   Lists.subtitle [] [ text data.filePath ]
          ]
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
