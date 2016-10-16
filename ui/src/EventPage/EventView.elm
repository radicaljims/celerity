module EventView exposing (..)

import EventTypes exposing (Data)

import Bootstrap.Html exposing (..)
import Html exposing (..)
-- import Html.Attributes as A exposing (style, class)
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

import EventModel as Model exposing (Msg, Model, Mdl)
import EventStyles as Styles
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

fetch : Model -> Int -> Msg -> String -> Html Msg
fetch model idx action buttonText  =
  makeBusyButton model model.fetching idx action buttonText

eventList : Model -> Html Msg
eventList model =
  Lists.ul
      [ css "margin" "0", css "padding" "0"]
      (List.map listItem model.data)

view : Model -> Html Msg
view model =
  div []
      [ eventList model ]
    -- [ fetch model 0 Model.GetCopies "Get Events"
    -- , eventList model
    -- ]

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
