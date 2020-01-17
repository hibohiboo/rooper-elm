module Models.RoomData.Protagonist exposing (..)

import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.RoomData.Hand as Hand exposing (Hand)
import Models.TragedySet as TragedySet exposing (Incident, TragedySet)


type alias Protagonist =
    { number : Int
    , name : String
    , twitterScreenName : String
    , hands : List Hand
    }



-- ==============================================================================================
-- init
-- ==============================================================================================


init : String -> String -> String -> List Protagonist
init id1 id2 id3 =
    [ Protagonist 1 "主人公1" id1 (Hand.initProtagonist 1)
    , Protagonist 2 "主人公2" id2 (Hand.initProtagonist 2)
    , Protagonist 3 "主人公3" id3 (Hand.initProtagonist 3)
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
        |> Pipeline.required "hands" (D.list Hand.decoder)



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : Protagonist -> E.Value
encode { number, name, twitterScreenName, hands } =
    E.object
        [ ( "number", E.int number )
        , ( "name", E.string name )
        , ( "twitterScreenName", E.string twitterScreenName )
        , ( "hands", E.list Hand.encode hands )
        ]



-- ==============================================================================================
-- View
-- ==============================================================================================
