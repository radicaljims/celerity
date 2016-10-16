module EventModel exposing (..)

import Material

import EventComms exposing (fetchCopies)
import EventTypes exposing (Data)

import Models.Comms as Comm

-- Adds a 'Material.Model' to 'm'
type alias MdlModel m =
  { m | mdl : Material.Model }

type alias MdlMessage m = Material.Msg m

-- The empty record, wellspring of them all
type alias Empty = {}

type alias Model = MdlModel (Comm.Model (List Data) Empty)

emptyModel : Model
emptyModel =
  { data = []
  , message  = ""
  , fetching = False
  , mdl = Material.model }

init : (Model, Cmd a)
init = emptyModel ! []

type Msg =  NoOp
         | Comms (Comm.Msg (List Data))
         | Mdl (MdlMessage Msg)

getEvents : Cmd (Comm.Msg (List Data))
getEvents = fetchCopies Comm.GetDataSuccess Comm.GetDataFailure

foo msg model cmd u t =
    let
        (model', cmd) = u msg model cmd
    in
        (model', Cmd.map t cmd)

-- updater, action, resultor
foo2 u a r =
    let
        (model', cmd) = u a
    in
        (model', r cmd)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      NoOp -> model ! []

      Comms msg' ->
          let
              u = (Comm.update msg' model)
              r = (Cmd.map Comms)
          in
              foo2 u getEvents r
          -- foo msg' model getEvents (Comm.update) Comms
          -- let
          --   (model', cmd) = Comm.update msg' model getEvents
          -- in
          --   (model', Cmd.map Comms cmd)

      Mdl msg' ->
          Material.update msg' model

type alias Mdl = Material.Model
