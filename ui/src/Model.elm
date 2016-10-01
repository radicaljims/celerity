module Model exposing (..)

import Material

import DirectoryModel
import EventModel

type Tab = Events | Directories | Alerts

type alias Model =
    { directoryModel : DirectoryModel.Model
    , eventModel : EventModel.Model
    , mdl : Material.Model
    , activeTab : Tab
    }

emptyModel : Model
emptyModel =  { directoryModel = DirectoryModel.emptyModel
              , eventModel = EventModel.emptyModel
              , mdl = Material.model
              , activeTab = Directories
              }

init : (Model, Cmd a)
init = emptyModel ! []

type Msg =  NoOp
           | DM (DirectoryModel.Msg)
           | EM (EventModel.Msg)
           | ActiveTab Int
           | Mdl (Material.Msg Msg)

-- updateSubModel : Msg -> Model -> (Model, Cmd Msg)
-- updateSubModel (Con message) model =
--   let
--       ( updatedModel, cmd) =
--           update m model.directoryModel
--   in
--       ( { model | directoryModel = updatedDirectoryModel }, Cmd.map DM cmd)

update : Msg -> Model -> (Model, Cmd Msg)
update comm model =
  case comm of
      NoOp -> model ! []

      ActiveTab t ->
        let intToTab n = if n == 1 then Events else if n ==2 then Alerts else Directories
        in
          { model | activeTab = intToTab t} ! []

      DM massage ->
        let
            ( updatedDirectoryModel, cmd) =
                DirectoryModel.update massage model.directoryModel
        in
            ( { model | directoryModel = updatedDirectoryModel }, Cmd.map DM cmd)

      EM massage ->
        let
            ( updatedEventModel, cmd) =
                EventModel.update massage model.eventModel
        in
            ( { model | eventModel = updatedEventModel }, Cmd.map EM cmd)

      Mdl msg' ->
          Material.update msg' model

type alias Mdl = Material.Model
