module DirectoryView exposing (..)

-- import DirectoryTypes exposing (Directory)
import IndyApi exposing (WatchedDirectory)

import Bootstrap.Html exposing (..)
import Html exposing (..)
-- import Html.Attributes as A exposing (style, class)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)

import Material.Elevation as Elevation
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Card as Card
import Material.Color as Color
import Material.Icon as Icon
-- import Material.Layout as Layout
import Material.List as Lists
import Material.Progress as Progress
-- import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css, span)
import Material.Tooltip as Tooltip
import Material.Typography as Typography

import List exposing (..)
import Dict exposing (get)

import DirectoryModel as Model exposing (Msg, Model, Mdl)
-- import DirectoryStyles as Styles

view : Model -> Html Msg
view model =
  div []
    [ grid [] (List.indexedMap (card model) model.directories) ]

white : Options.Property c m
white = Color.text Color.white

blueGreyBG : Options.Property c m
blueGreyBG = Color.text (Color.color Color.BlueGrey Color.S300)

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
    if (Model.isShowingFiles model idx) then (size All 5) else (size All 3)

cellCssWidth model idx =
    if (Model.isShowingFiles model idx) then (css "width" "512px") else (css "width" "256px")

expander = Card.text [Card.expand] []

cardViewTop model idx directory =
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
  , expander
  ]

cardViewFiles model idx directory =
  [ Card.title
      []
      [
        Options.div [css "height" "350px", css "margin" "1px", css "width" "475px", css "overflow" "auto"] [eventList (Dict.get idx model.events)]
      ]
  -- , expander
  ]

eventList : Maybe IndyApi.FileSystem -> Html Msg
eventList mfs = case mfs of
  Nothing -> text "No files found"
  Just fs ->
    Lists.ul
        -- [ css "margin" "0", css "padding" "0", css "height" "auto", css "background" "white"]
        [ css "margin" "0", css "padding" "0", css "height" "auto"]
        (List.map listItem fs.files)

listItem : String -> Html Msg
listItem data =
  Lists.li
    [ ]
    [ Lists.content
        -- [Typography.caption, white]
        [white]
        [ Lists.avatarIcon "insert_drive_file" [Color.background (Color.color Color.DeepPurple Color.S300)], text data]
        -- [ Icon.view "insert_drive_file" [blueGreyBG], text data ]
        -- [ Icon.view "insert_drive_file" [blueGreyBG], text data ]
    ]

card : Model -> Int -> WatchedDirectory -> Material.Grid.Cell Msg
card model idx directory =
    let menuIcon = if (Model.isShowingFiles model idx) then "insert_drive_file" else "list"
        cardView = if (Model.isShowingFiles model idx) == False then (cardViewTop model idx directory)
                   else (cardViewFiles model idx directory)
    in
    cell [cellSize model idx]
      [ Card.view
          [ cellCssWidth model idx
          , css "height" "256px"
          -- , Color.background (Color.color Color.DeepPurple Color.S400)
          -- , Color.background (Color.color Color.Orange Color.S800)
          , Color.background (Color.color Color.BlueGrey Color.S300)
          , dynamic idx model
          , css "margin" "10px"
          , css "padding" "10px"
          ]
          (append cardView
          [Card.actions
              [Card.border, css "vertical-align" "bottom", css "text-align" "left", white
              -- , greyBackground
              ]
              [Button.render Mdl [idx + 100] model.mdl
                [ Button.icon, Button.ripple, white, Tooltip.attach Mdl [0]
                , Button.onClick (Model.ToggleFiles idx)]
                [Icon.i menuIcon]
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
          ])
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

fetch : Model -> Int -> Msg -> String -> Html Msg
fetch model idx action buttonText  =
  makeBusyButton model model.fetching idx action buttonText

iconNames : List (String, String)
iconNames = [("addormod", "note_add"), ("delete", "delete")]

iconColors : List (String, Color.Color)
iconColors = [("addormod", Color.color Color.BlueGrey Color.S300), ("delete", Color.color Color.Yellow Color.S800)]
