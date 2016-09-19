module Model exposing (..)

import Material
import Comms exposing (fetchCopies, fetchDirectories)
import Types exposing (Data, Directory)

type Tab = Events | Directories

type alias Model =
    { copies : List Data
    , message : String
    , mdl : Material.Model
    , fetching : Bool
    , activeTab : Tab
    , directories : List Directory
    }

emptyModel : Model
emptyModel =  { copies = [], message = "" , mdl = Material.model
              , fetching = False, directories = []
              , activeTab = Events
              }

init : (Model, Cmd a)
init = emptyModel ! []

type Msg =  NoOp | GetCopies | GetCopiesSuccess (List Data) | GetCopiesFailure String
           | ActiveTab Int
           | GetDirectories | GetDirectoriesSuccess (List Directory) | GetDirectoriesFailure String
           | Mdl (Material.Msg Msg)

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
          let intToTab n = if n == 0 then Events else Directories
          in
            { model | activeTab = intToTab t} ! []

      Mdl msg' ->
          Material.update msg' model

type alias Mdl = Material.Model
