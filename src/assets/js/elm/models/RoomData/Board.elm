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
