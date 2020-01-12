module Models.RoomData.MasterMind exposing (..)

import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.TragedySet as TragedySet exposing (Incident, TragedySet)


type alias MasterMind =
    { name : String
    , twitterScreenName : String
    }



-- ==============================================================================================
-- init
-- ==============================================================================================


init : String -> MasterMind
init id =
    MasterMind "脚本家" id



-- ==============================================================================================
-- メソッド
-- ==============================================================================================


isMasterMind : String -> MasterMind -> Bool
isMasterMind s m =
    m.twitterScreenName == s



-- ==============================================================================================
-- デコーダ
-- ==============================================================================================


decoder : D.Decoder MasterMind
decoder =
    D.succeed MasterMind
        |> Pipeline.required "name" D.string
        |> Pipeline.required "twitterScreenName" D.string



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : MasterMind -> E.Value
encode p =
    E.object
        [ ( "name", E.string p.name )
        , ( "twitterScreenName", E.string p.twitterScreenName )
        ]



-- ==============================================================================================
-- View
-- ==============================================================================================
