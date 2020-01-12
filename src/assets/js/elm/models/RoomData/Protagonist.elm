module Models.RoomData.Protagonist exposing (..)

import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.TragedySet as TragedySet exposing (Incident, TragedySet)


type alias Protagonist =
    { number : Int
    , name : String
    , twitterScreenName : String
    }



-- ==============================================================================================
-- init
-- ==============================================================================================


init : Int -> String -> Maybe Protagonist
init num id =
    case num of
        1 ->
            Just <| Protagonist num "主人公1" id

        2 ->
            Just <| Protagonist num "主人公2" id

        3 ->
            Just <| Protagonist num "主人公3" id

        _ ->
            Nothing



-- ==============================================================================================
-- デコーダ
-- ==============================================================================================


decoder : D.Decoder Protagonist
decoder =
    D.succeed Protagonist
        |> Pipeline.required "number" D.int
        |> Pipeline.required "name" D.string
        |> Pipeline.required "twitterScreenName" D.string



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : Protagonist -> E.Value
encode p =
    E.object
        [ ( "number", E.int p.number )
        , ( "name", E.string p.name )
        , ( "twitterScreenName", E.string p.twitterScreenName )
        ]



-- ==============================================================================================
-- View
-- ==============================================================================================
