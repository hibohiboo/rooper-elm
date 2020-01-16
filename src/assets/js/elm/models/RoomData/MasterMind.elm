module Models.RoomData.MasterMind exposing (..)

import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (class, href, src, style)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.RoomData.Hand as Hand exposing (Hand)
import Models.TragedySet as TragedySet exposing (Incident, TragedySet)


type alias MasterMind =
    { name : String
    , twitterScreenName : String
    , hands : List Hand
    }



-- ==============================================================================================
-- init
-- ==============================================================================================


init : String -> MasterMind
init id =
    MasterMind "脚本家" id Hand.initMastermind



-- ==============================================================================================
-- メソッド
-- ==============================================================================================
-- ==============================================================================================
-- setter
-- ==============================================================================================


changeMasterMindHand : Int -> String -> MasterMind -> MasterMind
changeMasterMindHand i s f =
    { f | hands = Hand.changeMasterMindHand i s f.hands }



-- ==============================================================================================
-- getter
-- ==============================================================================================


isMasterMind : String -> MasterMind -> Bool
isMasterMind s m =
    m.twitterScreenName == s


getSelectedHandComponentKey : Int -> MasterMind -> String
getSelectedHandComponentKey i m =
    Hand.getSelectedHandComponentKey i m.hands



-- ==============================================================================================
-- デコーダ
-- ==============================================================================================


decoder : D.Decoder MasterMind
decoder =
    D.succeed MasterMind
        |> Pipeline.required "name" D.string
        |> Pipeline.required "twitterScreenName" D.string
        |> Pipeline.required "hands" (D.list Hand.decoder)



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : MasterMind -> E.Value
encode { name, twitterScreenName, hands } =
    E.object
        [ ( "name", E.string name )
        , ( "twitterScreenName", E.string twitterScreenName )
        , ( "hands", E.list Hand.encode hands )
        ]



-- ==============================================================================================
-- View
-- ==============================================================================================


handsForm : Int -> MasterMind -> (String -> msg) -> Html msg
handsForm i master chgMsg =
    let
        key =
            case Hand.getSelectedHand i master.hands of
                Just h ->
                    h.id

                Nothing ->
                    ""

        optionList =
            Hand.getFormOptionList i master.hands
    in
    Form.select ("form-mastermind-hand-" ++ String.fromInt i) chgMsg key optionList


selectedCard : Int -> MasterMind -> Html msg
selectedCard i master =
    case Hand.getSelectedHand i master.hands of
        Just h ->
            img [ src <| Hand.toCardUrl h ] []

        Nothing ->
            text ""
