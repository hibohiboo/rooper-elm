module Models.RoomData.Board exposing (..)

import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Json.Encode.Extra as ExEncode
import Models.Board as Board exposing (BoardType)
import Models.TragedySet as TragedySet exposing (Role)
import Models.Utility.List as UtilityList


type alias Board =
    { boardType : BoardType
    , name : String
    , intrigue : Int
    }



-- ==============================================================================================
-- init
-- ==============================================================================================


init : List Board
init =
    [ Board Board.shrine.boardType Board.shrine.name 0
    , Board Board.school.boardType Board.school.name 0
    , Board Board.hospital.boardType Board.hospital.name 0
    , Board Board.city.boardType Board.city.name 0
    ]



-- ==============================================================================================
-- setter
-- ==============================================================================================


setIntrigue : Int -> Board -> Board
setIntrigue v c =
    { c | intrigue = v }



-- ==============================================================================================
-- デコーダ
-- ==============================================================================================


decoder : D.Decoder Board
decoder =
    D.succeed Board
        |> Pipeline.required "boardType" (D.map (Board.boardTypeFromString >> Maybe.withDefault Board.Shrine) D.string)
        |> Pipeline.required "name" D.string
        |> Pipeline.required "intrigue" D.int



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : Board -> E.Value
encode { boardType, name, intrigue } =
    E.object
        [ ( "name", E.string name )
        , ( "boardType", E.string <| Board.boardTypeToString boardType )
        , ( "intrigue", E.int intrigue )
        ]



-- ==============================================================================================
-- View
-- ==============================================================================================
-- ==============================================================================================
-- View
-- ==============================================================================================


boardCard : Board -> Html msg
boardCard b =
    div [ class "rooper-roomdata-board-card" ]
        [ img [ src (Board.boardToCardUrl b.boardType) ] []
        , boardCardChip b.intrigue "intrigue"
        ]


boardCardChip : Int -> String -> Html msg
boardCardChip i s =
    div [ class "rooper-roomdata-board-parameter" ] <|
        List.concat
            [ List.map
                (\_ -> span [ class <| "chip big " ++ s ] [ text "3" ])
                (List.range 1 (i // 3))
            , List.map
                (\_ -> span [ class <| "chip " ++ s ] [ text "" ])
                (List.range 1 (modBy 3 i))
            ]


boardsFormItem : Board -> (String -> msg) -> Html msg
boardsFormItem b changeIMsg =
    div []
        [ div [ class "rooper-board-room-form-item" ]
            [ div []
                [ boardCard b
                , div [] [ text b.name ]
                ]
            , div []
                [ text "暗躍"
                , div []
                    [ input [ value <| String.fromInt b.intrigue, onChange changeIMsg, type_ "number" ] []
                    ]
                ]
            ]
        ]
