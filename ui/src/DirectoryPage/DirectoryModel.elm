module DirectoryModel exposing (..)

import Material
import DirectoryComms exposing (fetchDirectories)
import DirectoryTypes exposing (Directory)

import EventModel

import Dict exposing (Dict)

type alias Model =
  { message : String
  , mdl : Material.Model
  , directories : List Directory
  , raised : Int
  , events : Dict Int EventModel.Model
  , fetching : Bool
  }

emptyModel : Model
emptyModel =
  { message = ""
  , mdl = Material.model
  , directories = []
  , raised = -1
  , events = Dict.empty
  , fetching = False
  }

init : (Model, Cmd a)
init = emptyModel ! []

type Msg =  NoOp
           | GetDirectories
           | GetDirectoriesSuccess (List Directory)
           | GetDirectoriesFailure String
           | Raise Int
           | ToggleFiles Int
           | Mdl (Material.Msg Msg)

getDirectories : Cmd Msg
getDirectories = fetchDirectories GetDirectoriesSuccess GetDirectoriesFailure

isShowingFiles m idx = Dict.member idx m.events

update : Msg -> Model -> (Model, Cmd Msg)
update comm model =
  case comm of
    NoOp -> model ! []

    GetDirectories ->
      { model | message = "Fetching directories...", fetching = True, events = Dict.empty } ! [getDirectories]

    GetDirectoriesSuccess directories ->
      { model | message = "Fetched directories", directories = directories, fetching = False, events = Dict.empty} ! []

    GetDirectoriesFailure error ->
      { model | message = error, fetching = False, events = Dict.empty } ! []

    Raise k ->
      { model | raised = k } ! []

    ToggleFiles idx ->
      let getFiles = \_ -> []
          stopShowing model idx = Dict.remove idx model.events
      in
        { model | events = if (Dict.member idx model.events) then (Dict.remove idx model.events) else (Dict.insert idx EventModel.emptyModel model.events) }
            ! [] -- TODO: probably actually stop using EventModel here and just hit a (possibly still to be mocked!) /directories/files endpoint!
                 -- buuuuut probably that will be a model in it's own right to support additional interactivity! Who knows! I need practice composing models anyway.

    Mdl msg' ->
      Material.update msg' model

type alias Mdl = Material.Model
