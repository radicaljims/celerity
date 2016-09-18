module Model exposing (..)

import Material
import Comms exposing (fetchCopies)
import Types exposing (Data)

type alias Model =
    { copies : List Data
    , message : String
    , mdl : Material.Model
    , fetching : Bool
    }

emptyModel : Model
emptyModel =  { copies = [], message = "" , mdl = Material.model
              , fetching = False
              }

init : (Model, Cmd a)
init = emptyModel ! []

type Msg =  NoOp | Get | GetSuccess (List Data) | GetFailure String
           | Mdl (Material.Msg Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update comm model =
  case comm of
      NoOp -> model ! []

      Get ->
          { model | message = "Fetching copies...", fetching = True } ! [fetchCopies GetSuccess GetFailure]

      GetSuccess copies ->
          { model | message = "Fetched copies", copies = copies, fetching = False} ! []

      GetFailure error ->
          { model | message = error, fetching = False } ! []

      Mdl msg' ->
          Material.update msg' model

type alias Mdl = Material.Model
