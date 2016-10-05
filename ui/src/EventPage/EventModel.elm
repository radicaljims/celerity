module EventModel exposing (..)

import Material

import EventComms exposing (fetchCopies)
import EventTypes exposing (Data)

-- type Tab = Events | Directories | Alerts

type alias Model =
    { copies : List Data
    , message : String
    , mdl : Material.Model
    , fetching : Bool
    }

emptyModel : Model
emptyModel =  { copies = []
              , message = ""
              , mdl = Material.model
              , fetching = False
              }

init : (Model, Cmd a)
init = emptyModel ! []

type Msg =  NoOp
         | GetCopies
         | GetCopiesSuccess (List Data)
         | GetCopiesFailure String
         | Mdl (Material.Msg Msg)


getEvents : Cmd Msg
getEvents = fetchCopies GetCopiesSuccess GetCopiesFailure

update : Msg -> Model -> (Model, Cmd Msg)
update comm model =
  case comm of
      NoOp -> model ! []

      GetCopies ->
          { model | message = "Fetching copies...", fetching = True } ! [getEvents]

      GetCopiesSuccess copies ->
          { model | message = "Fetched copies", copies = copies, fetching = False} ! []

      GetCopiesFailure error ->
          { model | message = error, fetching = False } ! []

      Mdl msg' ->
          Material.update msg' model

type alias Mdl = Material.Model
