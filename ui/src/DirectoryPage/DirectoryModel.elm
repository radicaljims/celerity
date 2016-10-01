module DirectoryModel exposing (..)

import Material
import DirectoryComms exposing (fetchDirectories)
import DirectoryTypes exposing (Directory)

type alias Model =
    { message : String
    , mdl : Material.Model
    , directories : List Directory
    , raised : Int
    , showFiles : List Int
    , fetching : Bool
    }

emptyModel : Model
emptyModel =  { message = ""
              , mdl = Material.model
              , directories = []
              , raised = -1
              , showFiles = []
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

type alias Mdl = Material.Model
