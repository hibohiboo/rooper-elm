module Models.RoomState exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Models.RoomData as RoomData exposing (RoomData)
import Models.RoomData.RoomDataState exposing (RoomDataState(..))
import Models.User exposing (User)



-- Core


type alias RoomState =
    { tabsState : TabsState
    , bottomNavOpen : Bool
    , turnProtagonistNumber : Int
    , isDisplayCardsAreResolved : Bool
    , tweetProtagonistNumber : Int
    }


type TabsState
    = Parameter
    | Action
    | Hand


init : RoomState
init =
    RoomState Action True 0 False 0


setBottomNav : Bool -> RoomState -> RoomState
setBottomNav b f =
    { f | bottomNavOpen = b }


setParameterTab : RoomState -> RoomState
setParameterTab f =
    { f | tabsState = Parameter }


setActionTab : RoomState -> RoomState
setActionTab f =
    { f | tabsState = Action }


setHandTab : RoomState -> RoomState
setHandTab f =
    { f | tabsState = Hand }


setFalseIsDisplayCardsAreResolved : RoomState -> RoomState
setFalseIsDisplayCardsAreResolved f =
    { f | isDisplayCardsAreResolved = False }


updateByRoomDataState : Maybe RoomData -> RoomState -> RoomState
updateByRoomDataState data state =
    state
        |> updateTabsStateByRoomDataState data
        |> updateTurnProtagonistByRoomDataState data
        |> updateIsDisplayCardsAreResolved data


updateTabsStateByRoomDataState : Maybe RoomData -> RoomState -> RoomState
updateTabsStateByRoomDataState data state =
    if RoomData.isRoomStateHand data then
        setHandTab state

    else if RoomData.isCardsAreResolvedState data then
        setParameterTab state

    else
        case data of
            Just d ->
                if d.state == TimeSpairal then
                    setParameterTab state

                else
                    state

            Nothing ->
                state


updateTurnProtagonistByRoomDataState : Maybe RoomData -> RoomState -> RoomState
updateTurnProtagonistByRoomDataState data state =
    case data of
        Just d ->
            { state | turnProtagonistNumber = RoomData.getTurnProtagonistNumber d }

        Nothing ->
            state


updateIsDisplayCardsAreResolved : Maybe RoomData -> RoomState -> RoomState
updateIsDisplayCardsAreResolved data state =
    if RoomData.isCardsAreResolvedState data then
        { state | isDisplayCardsAreResolved = True }

    else
        { state | isDisplayCardsAreResolved = False }


setTweetProtagonistNumber : Int -> RoomState -> RoomState
setTweetProtagonistNumber i s =
    { s | tweetProtagonistNumber = i }


updateTweetProtagonistNumber : Maybe RoomData -> Maybe User -> RoomState -> RoomState
updateTweetProtagonistNumber data user state =
    case user of
        Just u ->
            case data of
                Just d ->
                    if RoomData.isTurnProtagonist state.turnProtagonistNumber u d then
                        setTweetProtagonistNumber state.turnProtagonistNumber state

                    else
                        state

                Nothing ->
                    state

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
                class "navbar is-fixed-bottom rooper-room-wrapper__nav rooper-bottom-form is-active"

            else
                class "navbar is-fixed-bottom rooper-room-wrapper__nav rooper-bottom-form"
    in
    nav [ c ]
        [ div [ class "card" ] children ]


roomDataFormHeaderTitle : RoomState -> Html msg
roomDataFormHeaderTitle state =
    p [ class "card-header-title" ]
        [ text <|
            case state.tabsState of
                Action ->
                    "アクション"

                Parameter ->
                    "パラメータ"

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
