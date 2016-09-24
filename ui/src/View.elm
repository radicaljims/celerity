module View exposing (..)

import Types exposing (Data, Directory)

import Bootstrap.Html exposing (..)
import Html exposing (..)
import Html.Attributes as A exposing (style, class)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)

import Material.Elevation as Elevation
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Card as Card
import Material.Color as Color
import Material.Icon as Icon
import Material.Layout as Layout
import Material.List as Lists
import Material.Progress as Progress
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css, span)
import Material.Tooltip as Tooltip
import Material.Typography as Typography

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

iconColors : List (String, Color.Color)
iconColors = [("addormod", Color.color Color.BlueGrey Color.S300), ("delete", Color.color Color.Yellow Color.S800)]

view : Model -> Html Msg
view model =
  Layout.render Mdl
    model.mdl
      [ Layout.waterfall True
      , Layout.fixedHeader
      , Layout.rippleTabs
      , Layout.onSelectTab Model.ActiveTab
      ]
      { header = [ header ]
      , drawer = []
      , tabs = ( [text "Directories", text "Event Log", text "Alerts"], [] )
      , main = [body model]
      }

  |> Material.Scheme.topWithScheme Color.BlueGrey Color.Indigo

header : Html Msg
header =
  div
    []
    [ h3 [style Styles.header] [text "Celerity"]]

fetch : Model -> Int -> Msg -> String -> Html Msg
fetch model idx action buttonText  =
  makeBusyButton model model.fetching idx action buttonText

body : Model -> Html Msg
body model =
  case model.activeTab of

    Model.Events ->
      div []
          [ fetch model 0 Model.GetCopies "Get Events",
            Lists.ul
              [ css "margin" "0", css "padding" "0"]
              (List.map listItem model.copies)
          ]

    Model.Directories ->
      div []
          (append
          [ fetch model 1 Model.GetDirectories "Get Directories" ]
          [ grid [] (List.indexedMap (card model) model.directories) ]
          )

    Model.Alerts ->
        text "No alerts yet buddy!"


listItem : Data -> Html Msg
listItem data =
  let prettyType = lookup prettyTypes data.eventType data.eventType
      iconColor = lookup iconColors data.eventType (Color.color Color.Red Color.S900)
  in
    Lists.li
      [ Lists.withSubtitle ]
      [ Lists.content
          []
          [ Lists.avatarIcon
              "insert_drive_file" [Color.background iconColor]
          ,   text prettyType
          ,   Lists.subtitle [] [ text data.filePath ]
          ]
      ]

white : Options.Property c m
white = Color.text Color.white

greyBackground : Options.Property c m
greyBackground = Color.background (Color.color Color.Grey Color.S400)

dynamic : Int -> Model -> Options.Style Msg
dynamic k model =
  [ if model.raised == k then Elevation.e8 else Elevation.e2
    , Elevation.transition 250
    , Options.attribute <| onMouseEnter (Model.Raise k)
    , Options.attribute <| onMouseLeave (Model.Raise -1)
    ] |> Options.many

cellSize model idx =
  let isShowingFiles =
    List.isEmpty (List.filter (\x -> x == idx) model.showFiles) /= True
  in
    if isShowingFiles then (size All 5) else (size All 3)

cellCssWidth model idx =
  let isShowingFiles =
    List.isEmpty (List.filter (\x -> x == idx) model.showFiles) /= True
  in
    if isShowingFiles then (css "width" "512px") else (css "width" "256px")
card : Model -> Int -> Directory -> Material.Grid.Cell Msg
card model idx directory =
    -- cell [size All 3]
    cell [cellSize model idx]
      [ Card.view
          -- [ css "width" "256px"
          [ cellCssWidth model idx
          , css "height" "256px"
          , Color.background (Color.color Color.DeepPurple Color.S400)
          , dynamic idx model
          , css "margin" "10px"
          , css "padding" "10px"
          ]
          [ Card.title
              []
              [Options.div
                []
                [
                  Icon.view "insert_drive_file" [Icon.size48, white]
                , Options.div []
                    [ Card.head [ white ] [ text directory.shortName]
                    , Card.subhead [ white ] [ text ((toString directory.usedSpace) ++ " Bytes") ]
                    ]
                ]
              ]
          , Card.text [Card.expand] -- an expander
              []
          , Card.actions
              [Card.border, css "vertical-align" "bottom", css "text-align" "right", white
              -- , greyBackground
              ]
              [Button.render Mdl [idx + 100] model.mdl
                [ Button.icon, Button.ripple, white, Tooltip.attach Mdl [0]
                , Button.onClick (Model.ToggleFiles idx)]
                [Icon.i "list"]
              , Tooltip.render Mdl [0] model.mdl
                  []
                  [text "View files in directory"]
              , Button.render Mdl [idx + 200] model.mdl
                [Button.icon, Button.ripple, white, Tooltip.attach Mdl [1]]
                [Icon.i "delete"]
              , Tooltip.render Mdl [1] model.mdl
                  []
                  [text "Delete directory"]
              ]
          ]
      ]

makeButton : Model -> Int -> Msg -> String -> Html Msg
makeButton model idx action btnText =
  Button.render Mdl [idx] model.mdl
    [ Button.raised, Button.colored, Button.ripple, Button.onClick action, css "margin" "10px 10px 10px 10px"]
    [ text btnText ]

makeBusyButton : Model -> Bool -> Int -> Msg -> String -> Html Msg
makeBusyButton model isBusy idx action btnText =
  row_
    (append
      [makeButton model idx action btnText]
      (if isBusy then [ Progress.indeterminate ] else [])
    )
