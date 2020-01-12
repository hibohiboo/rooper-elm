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


init : String -> String -> String -> List Protagonist
init id1 id2 id3 =
    [ Protagonist 1 "主人公1" id1
    , Protagonist 2 "主人公2" id2
    , Protagonist 3 "主人公3" id3
    ]



-- ==============================================================================================
-- メソッド
-- ==============================================================================================


isProtagonist : String -> List Protagonist -> Bool
isProtagonist s list =
    list
        |> getUserProtagonists s
        |> List.length
        |> (>) 0


getUserProtagonists : String -> List Protagonist -> List Protagonist
getUserProtagonists s list =
    List.filter (\d -> d.twitterScreenName == s) list



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
