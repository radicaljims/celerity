module Models.Comms exposing (..)

type alias Model d m =
    { m | data : d, message : String, fetching : Bool }

type Msg d =
    GetData
    | GetDataSuccess d
    | GetDataFailure String

update msg model request  =
  case msg of
      GetData ->
          { model | message = "Fetching data...", fetching = True } ! [request]

      GetDataSuccess copies ->
          { model | message = "Fetched data", data = copies, fetching = False} ! []

      GetDataFailure error ->
          { model | message = error, fetching = False } ! []
