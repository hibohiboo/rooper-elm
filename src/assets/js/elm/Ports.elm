port module Ports exposing (..)

import Json.Encode exposing (Value)


port errorToJs : String -> Cmd msg


port signOut : () -> Cmd msg


port initLoginUI : () -> Cmd msg


port initSwiper : () -> Cmd msg


port updateRoom : Value -> Cmd msg


port readedRooms : (Value -> msg) -> Sub msg


port updateScenario : Value -> Cmd msg


port changeUrl : String -> Cmd msg


port changedUrl : (String -> msg) -> Sub msg


port readScenarioNames : () -> Cmd msg


port readedScenarioNames : (Value -> msg) -> Sub msg


port readScenario : String -> Cmd msg


port readedScenario : (Value -> msg) -> Sub msg


port deleteScenario : String -> Cmd msg


port deletedScenario : (Bool -> msg) -> Sub msg
