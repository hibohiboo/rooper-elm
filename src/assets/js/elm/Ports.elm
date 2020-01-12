port module Ports exposing (..)

import Json.Encode exposing (Value)


port errorToJs : String -> Cmd msg


port signOut : () -> Cmd msg


port initLoginUI : () -> Cmd msg


port initSwiper : () -> Cmd msg


port updateRoom : Value -> Cmd msg


port readRooms : () -> Cmd msg


port readedRooms : (Value -> msg) -> Sub msg


port readRoom : String -> Cmd msg


port readedRoom : (Value -> msg) -> Sub msg


port updateScript : Value -> Cmd msg


port changeUrl : String -> Cmd msg


port changedUrl : (String -> msg) -> Sub msg


port readScriptNames : () -> Cmd msg


port readedScriptNames : (Value -> msg) -> Sub msg


port readScript : String -> Cmd msg


port readedScript : (Value -> msg) -> Sub msg


port readScriptForRoom : String -> Cmd msg


port readedScriptForRoom : (Value -> msg) -> Sub msg


port deleteScript : String -> Cmd msg


port deletedScript : (Bool -> msg) -> Sub msg


port listenRoomData : String -> Cmd msg


port readedRoomData : (Value -> msg) -> Sub msg


port readedRoomForRoomData : (Value -> msg) -> Sub msg


port updateRoomData : Value -> Cmd msg
