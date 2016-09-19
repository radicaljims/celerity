module View exposing (..)

import Types exposing (Data)

import Bootstrap.Html exposing (..)
import Html exposing (..)
import Html.Attributes as A exposing (style, class)
-- import Html.Shorthand exposing (..)

import Material.Color as Color
import Material.Layout as Layout
import Material.List as Lists
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

iconHues : List (String, Color.Hue)
iconHues = [("addormod", Color.LightGreen), ("delete", Color.Teal)]

view : Model -> Html Msg
view model =
  Layout.render Mdl
    model.mdl
      [ Layout.fixedHeader ]
      { header = [ header ]
      , drawer = []
      , tabs = ( [], [] )
      , main = [fetch model, body model]
      }

  |> Material.Scheme.topWithScheme Color.BlueGrey Color.Indigo

header : Html Msg
header =
  div
    []
    [ h4 [style Styles.header] [text "Celerity"]]

fetch : Model -> Html Msg
fetch model =
    makeBusyButton model model.fetching 0 Model.Get "Get Events"

body : Model -> Html Msg
body model =
  Lists.ul
    [ css "margin" "0", css "padding" "0"]
    (List.map listItem model.copies)

listItem : Data -> Html Msg
listItem data =
  let prettyType = lookup prettyTypes data.eventType data.eventType
      iconHue = lookup iconHues data.eventType Color.Orange
  in
    Lists.li
      [ Lists.withSubtitle ]
      [ Lists.content
          []
          [ Lists.avatarIcon
              "insert_drive_file" [Color.background (Color.color iconHue Color.S500)]
          ,   text prettyType
          ,   Lists.subtitle [] [ text data.filePath ]
          ]
      ]

makeButton : Model -> Int -> Msg -> String -> Html Msg
makeButton model idx action btnText =
  Button.render Mdl [idx] model.mdl
    [ Button.raised, Button.colored, Button.onClick action, css "margin" "10px 10px 10px 10px"]
    [ text btnText ]

makeBusyButton : Model -> Bool -> Int -> Msg -> String -> Html Msg
makeBusyButton model isBusy idx action btnText =
  row_
    (append
      [makeButton model idx action btnText]
      (if isBusy then [ Progress.indeterminate ] else [])
    )
