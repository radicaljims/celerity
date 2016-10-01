module View exposing (..)

import Html exposing (..)
import Html.App exposing (map)

import Material.Color as Color
import Material.Layout as Layout
import Material.Scheme

import EventView
import DirectoryView

import Model exposing (Msg, Model, Mdl)

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
    -- [ h3 [style Styles.header] [text "Celerity"]]
    [ h3 [] [text "Celerity"]]

body : Model -> Html Msg
body model =
  case model.activeTab of

    Model.Directories ->
        Html.App.map Model.DM (DirectoryView.view model.directoryModel)

    Model.Events ->
        Html.App.map Model.EM (EventView.view model.eventModel)
        -- EventView.view model

    Model.Alerts ->
        text "No alerts yet buddy!"


