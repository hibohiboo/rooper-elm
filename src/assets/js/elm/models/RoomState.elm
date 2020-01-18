module Models.RoomState exposing (..)

import Component.Form as Form
import Component.Link
import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.RoomData as RoomData exposing (RoomData)
import Models.Script as Script exposing (Script)
import Models.ScriptName as ScriptName exposing (ScriptName)
import Models.User exposing (User)



-- Core


type alias RoomState =
    { tabsState : TabsState
    , bottomNavOpen : Bool
    , turnProtagonistNumber : Int
    }


type TabsState
    = Character
    | Data
    | Hand


init : RoomState
init =
    RoomState Data True 0


setBottomNav : Bool -> RoomState -> RoomState
setBottomNav b f =
    { f | bottomNavOpen = b }


setCharacterTab : RoomState -> RoomState
setCharacterTab f =
    { f | tabsState = Character }


setDataTab : RoomState -> RoomState
setDataTab f =
    { f | tabsState = Data }


setHandTab : RoomState -> RoomState
setHandTab f =
    { f | tabsState = Hand }


updateByRoomDataState : Maybe RoomData -> RoomState -> RoomState
updateByRoomDataState data state =
    state
        |> updateTabsStateByRoomDataState data
        |> updateTurnProtagonistByRoomDataState data


updateTabsStateByRoomDataState : Maybe RoomData -> RoomState -> RoomState
updateTabsStateByRoomDataState data state =
    if RoomData.isRoomStateHand data then
        setHandTab state

    else if RoomData.isParameterStateHand data then
        setCharacterTab state

    else
        state


updateTurnProtagonistByRoomDataState : Maybe RoomData -> RoomState -> RoomState
updateTurnProtagonistByRoomDataState data state =
    case data of
        Just d ->
            { state | turnProtagonistNumber = RoomData.getTurnProtagonistNumber d }

        Nothing ->
            state



-- ==============================================================================================
-- View
-- ==============================================================================================


roomDataBottomForm : RoomState -> List (Html msg) -> Html msg
roomDataBottomForm state children =
    let
        c =
            if state.bottomNavOpen then
                class "navbar is-fixed-bottom rooper-bottom-form is-active"

            else
                class "navbar is-fixed-bottom rooper-bottom-form"
    in
    nav [ c ]
        [ div [ class "card" ] children ]


roomDataFormHeaderTitle : RoomState -> Html msg
roomDataFormHeaderTitle state =
    p [ class "card-header-title" ]
        [ text <|
            case state.tabsState of
                Data ->
                    "データボード"

                Character ->
                    "キャラクター"

                Hand ->
                    "手札"
        ]


roomDataFormContent : List (Html msg) -> Html msg
roomDataFormContent children =
    div [ class "card-content" ]
        [ div [ class "content" ] children
        ]


roomDataFormFooter : List (Html msg) -> Html msg
roomDataFormFooter children =
    footer [ class "card-footer" ] children


roomDataFormDataBoard : List (Html msg) -> Html msg
roomDataFormDataBoard children =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ --   th [] [ text "Loop" ]
                  -- , th [] [ text "Date" ]
                  -- ,
                  th [] [ text "Exカウンタ" ]
                ]
            ]
        , tbody []
            [ tr [] children
            ]
        ]


roomDataFormHeaderIcon : msg -> msg -> RoomState -> Html msg
roomDataFormHeaderIcon close open state =
    span
        [ class "card-header-icon"
        , onClick <|
            if state.bottomNavOpen then
                close

            else
                open
        ]
        [ span [ class "icon" ]
            [ if state.bottomNavOpen then
                text "▼"

              else
                text "▲"
            ]
        ]
