module Models.RoomData.MasterMind exposing (..)

import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (src)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.RoomData.Hand as Hand exposing (Hand)


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
    { f | hands = Hand.changeHand i s f.hands }


changeMasterMindComponent : Int -> String -> MasterMind -> MasterMind
changeMasterMindComponent i s f =
    { f | hands = Hand.changeComponent i s f.hands }


returnPlayedHands : MasterMind -> MasterMind
returnPlayedHands f =
    { f | hands = Hand.changeHand 1 "m0" <| Hand.changeHand 2 "m1" <| Hand.changeHand 3 "m2" <| Hand.returnPlayedHands f.hands }



-- ==============================================================================================
-- getter
-- ==============================================================================================


isMasterMind : String -> MasterMind -> Bool
isMasterMind s m =
    m.twitterScreenName == s


getSelectedHandComponentKey : Int -> MasterMind -> String
getSelectedHandComponentKey i m =
    Hand.getSelectedHandComponentKey i m.hands


getPlayedHands : MasterMind -> List Hand
getPlayedHands m =
    Hand.playedHands m.hands



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
            img [ src "/assets/images/hands/Unselected.png" ] []


selectedComponentCard : Int -> MasterMind -> Html msg
selectedComponentCard i master =
    case Hand.getSelectedHand i master.hands of
        Just h ->
            img [ src <| Hand.toComponentCardUrl h ] []

        Nothing ->
            img [ src "/assets/images/hands/Unselected.png" ] []


useCardView : MasterMind -> Html msg
useCardView { hands } =
    div [] <|
        img [ src "/assets/images/hands/mastermind.png" ] []
            :: List.map (\h -> img [ src <| Hand.toCardUrl h ] [])
                (Hand.usedHands hands)
