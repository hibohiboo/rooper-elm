port module Ports exposing (..)

import Json.Encode exposing (Value)


port errorToJs : String -> Cmd msg


port signOut : () -> Cmd msg


port initLoginUI : () -> Cmd msg


port initSwiper : () -> Cmd msg


port updateRoom : Value -> Cmd msg
