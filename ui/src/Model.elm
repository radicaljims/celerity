module Model exposing (..)

import Material
import Comms exposing (fetchCopies, fetchDirectories)
import Types exposing (Directory)

import EventModel

type Tab = Events | Directories | Alerts

type alias Model =
    { message : String
    , mdl : Material.Model
    , fetching : Bool
    , activeTab : Tab
    , directories : List Directory
    , raised : Int -- getting annoyed at all the UI state here
    , showFiles : List Int
    , eventModel : EventModel.Model
    }

emptyModel : Model
emptyModel =  { message = "" , mdl = Material.model
              , fetching = False, directories = []
              , activeTab = Directories
              , raised = -1
              , showFiles = []
              , eventModel = EventModel.emptyModel
              }

init : (Model, Cmd a)
init = emptyModel ! []

type Msg =  NoOp
           | ActiveTab Int
           | GetDirectories | GetDirectoriesSuccess (List Directory) | GetDirectoriesFailure String
           | Raise Int
           | ToggleFiles Int
           | Mdl (Material.Msg Msg)
           | EM (EventModel.Msg)

isShowingFiles : Model -> Int -> Bool
isShowingFiles model idx =
    List.isEmpty (List.filter (\x -> x == idx) model.showFiles) /= True

update : Msg -> Model -> (Model, Cmd Msg)
update comm model =
  case comm of
      NoOp -> model ! []

      GetDirectories ->
          { model | message = "Fetching directories...", fetching = True } ! [fetchDirectories GetDirectoriesSuccess GetDirectoriesFailure]

      GetDirectoriesSuccess directories ->
          { model | message = "Fetched directories", directories = directories, fetching = False} ! []

      GetDirectoriesFailure error ->
          { model | message = error, fetching = False } ! []

      ActiveTab t ->
          let intToTab n = if n == 1 then Events else if n ==2 then Alerts else Directories
          in
            { model | activeTab = intToTab t} ! []

      Raise k ->
          { model | raised = k } ! []

      ToggleFiles idx ->
          let stopShowing model idx = List.filter (\x -> x /= idx) model.showFiles
              action = \_ -> []
              -- action = if (isShowingFiles model idx) == False then (\_ -> [fetchCopies GetCopiesSuccess GetCopiesFailure]) else
              --              (\_ -> [])
          in
            { model | showFiles = if (isShowingFiles model idx)
                                  then (stopShowing model idx) else (idx :: model.showFiles)} ! action()

      Mdl msg' ->
          Material.update msg' model

      EM massage ->
        let
            ( updatedEventModel, cmd) =
                EventModel.update massage model.eventModel
        in
            ( { model | eventModel = updatedEventModel }, Cmd.map EM cmd)

type alias Mdl = Material.Model
