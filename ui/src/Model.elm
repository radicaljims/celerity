module Model exposing (..)

import Material
import Comms exposing (fetchCopies, fetchDirectories)
import Types exposing (Data, Directory)

type Tab = Events | Directories | Alerts

type alias Model =
    { copies : List Data
    , message : String
    , mdl : Material.Model
    , fetching : Bool
    , activeTab : Tab
    , directories : List Directory
    , raised : Int -- getting annoyed at all the UI state here
    , showFiles : List Int
    }

emptyModel : Model
emptyModel =  { copies = [], message = "" , mdl = Material.model
              , fetching = False, directories = []
              , activeTab = Directories
              , raised = -1
              , showFiles = []
              }

init : (Model, Cmd a)
init = emptyModel ! []

type Msg =  NoOp | GetCopies | GetCopiesSuccess (List Data) | GetCopiesFailure String
           | ActiveTab Int
           | GetDirectories | GetDirectoriesSuccess (List Directory) | GetDirectoriesFailure String
           | Raise Int
           | ToggleFiles Int
           | Mdl (Material.Msg Msg)

isShowingFiles : Model -> Int -> Bool
isShowingFiles model idx =
    List.isEmpty (List.filter (\x -> x == idx) model.showFiles) /= True

update : Msg -> Model -> (Model, Cmd Msg)
update comm model =
  case comm of
      NoOp -> model ! []

      GetCopies ->
          { model | message = "Fetching copies...", fetching = True } ! [fetchCopies GetCopiesSuccess GetCopiesFailure]

      GetCopiesSuccess copies ->
          { model | message = "Fetched copies", copies = copies, fetching = False} ! []

      GetCopiesFailure error ->
          { model | message = error, fetching = False } ! []

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
          in
            { model | showFiles = if (isShowingFiles model idx)
                                  then (stopShowing model idx) else (idx :: model.showFiles)} ! []

      Mdl msg' ->
          Material.update msg' model

type alias Mdl = Material.Model
