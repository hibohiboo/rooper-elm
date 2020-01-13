module Models.Room exposing (..)

import Component.Form as Form
import Component.Link
import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, href, style)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.Script as Script exposing (Script)
import Models.ScriptName as ScriptName exposing (ScriptName)



-- Core


type alias RoomState =
    { tabsState : TabsState
    , bottomNavOpen : Bool
    }


type TabsState
    = Character
    | Game


init : RoomState
init =
    RoomState Game True
