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
import List.Extra as ExList
import Models.Board as Board exposing (BoardType)
import Models.RoomData.Hand as Hand exposing (Hand, HandType(..))
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
    [ shrine
    , Board Board.school.boardType Board.school.name 0
    , Board Board.hospital.boardType Board.hospital.name 0
    , Board Board.city.boardType Board.city.name 0
    ]


shrine : Board
shrine =
    Board Board.shrine.boardType Board.shrine.name 0



-- ==============================================================================================
-- setter
-- ==============================================================================================


setIntrigue : Int -> Board -> Board
setIntrigue v c =
    { c | intrigue = v }


resolveCard : List Hand -> Board -> Board
resolveCard hands b =
    let
        -- _ =
        --     Debug.log "decodeUser" hands
        list =
            List.map .handType <| Hand.getSelectedBoardHands b.boardType hands
    in
    if (==) 0 <| List.length list then
        b

    else if
        ((==) 1 <| List.length <| List.filter (\t -> t == ForbidIntrigue) <| list)
            && ((==) 1 <| List.length <| List.filter (\t -> t == ForbidIntrigue) <| List.map .handType <| hands)
    then
        b

    else if List.member IntriguePlus1 list then
        setIntrigue (b.intrigue + 1) b

    else if List.member IntriguePlus2 list then
        setIntrigue (b.intrigue + 2) b

    else
        b



-- ==============================================================================================
-- getter
-- ==============================================================================================


getBoard : BoardType -> List Board -> Board
getBoard t list =
    ExList.find (\b -> b.boardType == t) list
        |> Maybe.withDefault shrine


getHospital : List Board -> Board
getHospital list =
    getBoard Board.Hospital list


getCity : List Board -> Board
getCity list =
    getBoard Board.City list


getShrine : List Board -> Board
getShrine list =
    getBoard Board.Shrine list


getSchool : List Board -> Board
getSchool list =
    getBoard Board.School list


getFormOptionList : List BoardType -> List Board -> List ( String, String )
getFormOptionList slectedTypes list =
    list
        |> List.filter (\b -> not <| List.member b.boardType slectedTypes)
        |> List.map (\b -> ( Board.boardTypeToString b.boardType, b.name ))



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


boardCard : Board -> Bool -> Html msg
boardCard b isTurf =
    div [ class "rooper-roomdata-board-card" ]
        [ img [ src (Board.boardToCardUrl b.boardType) ] []
        , if isTurf then
            div [ style "height" "30px" ]
                [ img [ src "/assets/images/others/turf.png", style "transform" "scale(0.8)" ] []
                ]

          else
            text ""
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
                [ boardCard b False
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
