port module Ports exposing (..)


port errorToJs : String -> Cmd msg


port signOut : () -> Cmd msg


port initLoginUI : () -> Cmd msg
