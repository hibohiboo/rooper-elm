port module Ports exposing (..)

import Json.Encode exposing (Value)


port errorToJs : String -> Cmd msg


port signOut : () -> Cmd msg


port initLoginUI : () -> Cmd msg


port initSwiper : () -> Cmd msg


port updateRoom : Value -> Cmd msg


port readRooms : (Value -> msg) -> Sub msg


port updateScenario : Value -> Cmd msg


port changeUrl : String -> Cmd msg


port changedUrl : (String -> msg) -> Sub msg
