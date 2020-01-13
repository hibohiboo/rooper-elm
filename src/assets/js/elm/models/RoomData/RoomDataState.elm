module Models.RoomData.RoomDataState exposing (..)

import Component.Form as Form
import Component.Link
import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.Script as Script exposing (Script)
import Models.ScriptName as ScriptName exposing (ScriptName)



-- Core


type RoomDataState
    = InitLoop
    | PreTimeSpairal


init : RoomDataState
init =
    InitLoop


toString : RoomDataState -> String
toString state =
    case state of
        InitLoop ->
            "InitLoop"

        PreTimeSpairal ->
            "PreTimeSpairal"


fromString : String -> Maybe RoomDataState
fromString s =
    case s of
        "InitLoop" ->
            Just InitLoop

        "PreTimeSpairal" ->
            Just PreTimeSpairal

        _ ->
            Nothing


toName : RoomDataState -> String
toName state =
    case state of
        InitLoop ->
            "ループの準備"

        PreTimeSpairal ->
            "時の狭間"


fromStringWithDefault : String -> RoomDataState
fromStringWithDefault =
    fromString >> Maybe.withDefault PreTimeSpairal


nextState : RoomDataState -> RoomDataState
nextState state =
    case state of
        InitLoop ->
            PreTimeSpairal

        PreTimeSpairal ->
            InitLoop


decoder : D.Decoder RoomDataState
decoder =
    D.map fromStringWithDefault D.string


encode : RoomDataState -> E.Value
encode s =
    E.string <| toString s



-- ==============================================================================================
-- View
-- ==============================================================================================
