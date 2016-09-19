module View exposing (..)

import Types exposing (Data, Directory)

import Bootstrap.Html exposing (..)
import Html exposing (..)
import Html.Attributes as A exposing (style, class)

import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Card as Card
import Material.Color as Color
import Material.Layout as Layout
import Material.List as Lists
import Material.Progress as Progress
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css, span)
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
      , tabs = ( [text "Events", text "Directories"], [] )
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
          [ grid [] (List.map card model.directories) ]
          )

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

card : Directory -> Material.Grid.Cell a
card directory =
    cell [size All 3]
      [ Card.view
          [ css "width" "256px"
          , css "height" "256px"
          , Color.background (Color.color Color.DeepPurple Color.S400)
          , css "margin" "10px"
          , css "padding" "10px"
          ]
          [ Card.title
              [ css "align-content" "flex-start"
              , css "flex-direction" "row"
              , css "align-items" "flex-start"
              , css "justify-content" "space-between"
              ]
              [ Options.div
                  []
                  [ Card.head [ white ] [ text directory.directoryPath ]
                  , Card.subhead [ white ] [ text "Using a lot of space!" ]
                  ]
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
