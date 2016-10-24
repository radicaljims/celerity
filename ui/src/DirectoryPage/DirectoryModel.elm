module DirectoryModel exposing (..)

import Material

import IndyApi exposing (WatchedDirectory, getDirectories, FileSystem, getStatusByDirectory)

import Dict exposing (Dict)
import Task exposing (Task)

type alias Model =
  { message : String
  , mdl : Material.Model
  , directories : List WatchedDirectory
  , raised : Int
  , events : Dict Int FileSystem
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
           | GetDirectoriesSuccess (List WatchedDirectory)
           | GetDirectoriesFailure String
           -- | GetFiles Int
           | GetFilesSuccess Int FileSystem
           | GetFilesFailure String
           | Raise Int
           | ToggleFiles Int
           | Mdl (Material.Msg Msg)

task : Task a b -> (b -> c) -> (String -> c) -> Cmd c
task t s f = t |> Task.mapError toString |> Task.perform f s

task2 : Task a b -> (b -> c) -> (String -> c) -> Cmd c
task2 t s f = t |> Task.mapError toString |> Task.perform f s

fetchDirectories : Cmd Msg
fetchDirectories = task IndyApi.getDirectories GetDirectoriesSuccess GetDirectoriesFailure

fetchFiles : String -> Int -> Cmd Msg
fetchFiles directory id = task2 (IndyApi.getStatusByDirectory directory) (GetFilesSuccess id) GetFilesFailure

isShowingFiles m idx = Dict.member idx m.events

update : Msg -> Model -> (Model, Cmd Msg)
update comm model =
  case comm of
    NoOp -> model ! []

    GetDirectories ->
      { model | message = "Fetching directories...", fetching = True, events = Dict.empty } ! [fetchDirectories]

    GetDirectoriesSuccess directories ->
      { model | message = "Fetched directories", directories = directories, fetching = False, events = Dict.empty} ! []

    GetDirectoriesFailure error ->
      { model | message = error, fetching = False, events = Dict.empty } ! []

    -- GetFiles id ->
    --   let
    --     at : List String -> Int -> String
    --     at l n = case (List.head (List.drop (n - 1) l)) of
    --                  Nothing -> "lookup failed!"
    --                  Just h -> h
    --   in
    --     { model | message = "Fetching files...", fetching = True } ! [fetchFiles (at model.directories id) id]

    GetFilesSuccess id files ->
      { model | message = "Fetched files", fetching = False, events = Dict.insert id files model.events} ! []

    GetFilesFailure error ->
      { model | message = error, fetching = False, events = Dict.empty } ! []

    Raise k ->
      { model | raised = k } ! []

    ToggleFiles idx ->
        let
          at l n = case (List.head (List.drop (n - 1) l)) of
                      Nothing -> "lookup failed!"
                      Just h -> h.shortName
          action = if (Dict.member idx model.events) then [] else [fetchFiles (at model.directories idx) idx]
        in
          { model | events = if (Dict.member idx model.events) then (Dict.remove idx model.events) else (Dict.insert idx { files = [] } model.events) }
          ! action

    Mdl msg' ->
      Material.update msg' model

type alias Mdl = Material.Model
