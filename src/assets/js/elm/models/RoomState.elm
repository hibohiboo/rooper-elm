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


setBottomNav : Bool -> RoomState -> RoomState
setBottomNav b f =
    { f | bottomNavOpen = b }


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
                text "▲"

              else
                text "▼"
            ]
        ]
