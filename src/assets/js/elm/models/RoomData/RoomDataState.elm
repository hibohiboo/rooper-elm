module Models.RoomData.RoomDataState exposing (..)

import Html exposing (..)
import Json.Decode as D
import Json.Encode as E



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
    | CardsAreResolved
    | MastemindAbilities
    | GoodwillAbilities
    | IncidentsHappen
    | SwitchLeader
    | Night
    | TimeSpairal
    | FinalGuess


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

        CardsAreResolved ->
            "CardsAreResolved"

        MastemindAbilities ->
            "MastemindAbilities"

        GoodwillAbilities ->
            "GoodwillAbilities"

        IncidentsHappen ->
            "IncidentsHappen"

        SwitchLeader ->
            "SwitchLeader"

        Night ->
            "Night"

        TimeSpairal ->
            "TimeSpairal"

        FinalGuess ->
            "FinalGuess"


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

        "MastemindAbilities" ->
            Just MastemindAbilities

        "ProtagonistsPlaysCard" ->
            Just ProtagonistsPlaysCard

        "CardsAreResolved" ->
            Just CardsAreResolved

        "GoodwillAbilities" ->
            Just GoodwillAbilities

        "IncidentsHappen" ->
            Just IncidentsHappen

        "SwitchLeader" ->
            Just SwitchLeader

        "Night" ->
            Just Night

        "TimeSpairal" ->
            Just TimeSpairal

        "FinalGuess" ->
            Just FinalGuess

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

        CardsAreResolved ->
            "行動解決フェイズ"

        MastemindAbilities ->
            "脚本家能力フェイズ"

        GoodwillAbilities ->
            "主人公能力フェイズ"

        IncidentsHappen ->
            "事件フェイズ"

        SwitchLeader ->
            "リーダー交代フェイズ"

        Night ->
            "ターン終了フェイズ"

        TimeSpairal ->
            "時の狭間"

        FinalGuess ->
            "最後の戦い"


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
            CardsAreResolved

        CardsAreResolved ->
            MastemindAbilities

        MastemindAbilities ->
            GoodwillAbilities

        GoodwillAbilities ->
            IncidentsHappen

        IncidentsHappen ->
            SwitchLeader

        SwitchLeader ->
            Night

        Night ->
            Morning

        TimeSpairal ->
            SetupCharacter

        FinalGuess ->
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
