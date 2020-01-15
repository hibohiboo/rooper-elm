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
    | SetupCharacter
    | SetupCounter
    | SetupHand
    | Morning
    | MastermindPlaysCards
    | ProtagonistsPlaysCard


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

        SetupCharacter ->
            "SetupCharacter"

        SetupCounter ->
            "SetupCounter"

        SetupHand ->
            "SetupHand"

        Morning ->
            "Morning"

        MastermindPlaysCards ->
            "MastermindPlaysCards"

        ProtagonistsPlaysCard ->
            "ProtagonistsPlaysCard"


fromString : String -> Maybe RoomDataState
fromString s =
    case s of
        "InitLoop" ->
            Just InitLoop

        "PreTimeSpairal" ->
            Just PreTimeSpairal

        "SetupCharacter" ->
            Just SetupCharacter

        "SetupCounter" ->
            Just SetupCounter

        "SetupHand" ->
            Just SetupHand

        "Morning" ->
            Just Morning

        "MastermindPlaysCards" ->
            Just MastermindPlaysCards

        "ProtagonistsPlaysCard" ->
            Just ProtagonistsPlaysCard

        _ ->
            Nothing


toName : RoomDataState -> String
toName state =
    case state of
        InitLoop ->
            "ループの準備"

        PreTimeSpairal ->
            "時の狭間"

        SetupCharacter ->
            "キャラクターの配置"

        SetupCounter ->
            "カウンタの除去・配置"

        SetupHand ->
            "手札の分配"

        Morning ->
            "ターン開始フェイズ"

        MastermindPlaysCards ->
            "脚本家行動フェイズ"

        ProtagonistsPlaysCard ->
            "主人公行動フェイズ"


fromStringWithDefault : String -> RoomDataState
fromStringWithDefault =
    fromString >> Maybe.withDefault PreTimeSpairal


nextState : RoomDataState -> RoomDataState
nextState state =
    case state of
        InitLoop ->
            PreTimeSpairal

        PreTimeSpairal ->
            SetupCharacter

        SetupCharacter ->
            SetupCounter

        SetupCounter ->
            SetupHand

        SetupHand ->
            Morning

        Morning ->
            MastermindPlaysCards

        MastermindPlaysCards ->
            ProtagonistsPlaysCard

        ProtagonistsPlaysCard ->
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
