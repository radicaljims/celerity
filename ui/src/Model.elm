module Model exposing (..)

import Material

import DirectoryModel exposing (fetchDirectories)
import EventModel exposing (getEvents)

type Tab = Events | Directories | Alerts

type alias Model =
  { directoryModel : DirectoryModel.Model
  , eventModel : EventModel.Model
  , mdl : Material.Model
  , activeTab : Tab
  }

emptyModel : Model
emptyModel =
  { directoryModel = DirectoryModel.emptyModel
  , eventModel = EventModel.emptyModel
  , mdl = Material.model
  , activeTab = Directories
  }

init : (Model, Cmd Msg)
-- TODO: let's see if we can just directly generate a GetDirectories event instead of exposing
-- getDirectories
init = emptyModel ! [Cmd.map DM fetchDirectories]

type Msg =
    NoOp
  | DM (DirectoryModel.Msg)
  | EM (EventModel.Msg)
  | ActiveTab Int
  | Mdl (Material.Msg Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update comm model =
  case comm of
    NoOp -> model ! []

    ActiveTab t ->
      let
        intToTab n = if n == 1 then Events else if n == 2 then Alerts else Directories
        -- TODO: find a better solution to this Cmd.map composition
        dator n = if n == 1 then (Cmd.map EM (Cmd.map EventModel.Comms getEvents)) else if n == 2 then Cmd.none else (Cmd.map DM fetchDirectories)
      in
        { model | activeTab = intToTab t} ! [dator t]

    DM massage ->
      let
        (updatedDirectoryModel, cmd) =
          DirectoryModel.update massage model.directoryModel
      in
        ({ model | directoryModel = updatedDirectoryModel }, Cmd.map DM cmd)

    EM massage ->
      let
        (updatedEventModel, cmd) =
          EventModel.update massage model.eventModel
      in
        ({ model | eventModel = updatedEventModel }, Cmd.map EM cmd)

    Mdl msg' ->
      Material.update msg' model

type alias Mdl = Material.Model
