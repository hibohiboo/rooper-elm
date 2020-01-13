module Models.RoomState exposing (..)

import Component.Form as Form
import Component.Link
import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, href, style)
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


roomDataFormDataBoard : List (Html msg) -> Html msg
roomDataFormDataBoard children =
    div [ class "card-content" ]
        [ div [ class "content" ]
            [ table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Loop" ]
                        , th [] [ text "Date" ]
                        , th [] [ text "Ex" ]
                        ]
                    ]
                , tbody []
                    [ tr [] children
                    ]
                ]
            ]
        ]


roomDataFormHeaderIcon : Html msg
roomDataFormHeaderIcon =
    span [ class "card-header-icon" ]
        [ span [ class "icon" ]
            [ i [ class "fas fa-angle-down" ] []
            ]
        ]
