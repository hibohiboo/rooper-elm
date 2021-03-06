module Models.RoomData.Protagonist exposing (..)

import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import List.Extra as ExList
import Maybe.Extra as ExMaybe
import Models.RoomData.Hand as Hand exposing (Hand)


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


reset : List Protagonist -> List Protagonist
reset list =
    List.map (\p -> { p | hands = Hand.initProtagonist p.number }) list



-- ==============================================================================================
-- メソッド
-- ==============================================================================================
-- ==============================================================================================
-- setter
-- ==============================================================================================


changeProtagonistsHand : Int -> String -> List Protagonist -> List Protagonist
changeProtagonistsHand i s list =
    list
        |> List.map
            (\p ->
                if p.number == i then
                    changeProtagonistHand p.number s p

                else
                    p
            )


changeProtagonistsComponent : Int -> String -> List Protagonist -> List Protagonist
changeProtagonistsComponent i s list =
    list
        |> List.map
            (\p ->
                if p.number == i then
                    changeProtagonistComponent p.number s p

                else
                    p
            )


changeProtagonistHand : Int -> String -> Protagonist -> Protagonist
changeProtagonistHand i s f =
    { f | hands = Hand.changeHand i s f.hands }


changeProtagonistComponent : Int -> String -> Protagonist -> Protagonist
changeProtagonistComponent i s f =
    { f | hands = Hand.changeComponent i s f.hands }


returnPlayedHands : List Protagonist -> List Protagonist
returnPlayedHands list =
    list |> List.map (\p -> { p | hands = Hand.changeHand p.number "p0" <| Hand.returnPlayedHands p.hands })


changeLeader : List Protagonist -> List Protagonist
changeLeader list =
    case List.head list of
        Just h ->
            List.concat [ ExList.remove h list, [ h ] ]

        Nothing ->
            list


unusedProtagonistHand : Int -> Hand -> List Protagonist -> List Protagonist
unusedProtagonistHand i h list =
    list
        |> List.map
            (\p ->
                if p.number == i then
                    { p | hands = Hand.unusedProtagonistHand h p.hands }

                else
                    p
            )



-- ==============================================================================================
-- getter
-- ==============================================================================================


isProtagonistHandsPlayed : Int -> List Protagonist -> Bool
isProtagonistHandsPlayed i list =
    list
        |> List.filter (\p -> p.number == i)
        |> List.filter (\p -> Hand.isProtagonistHandsPlayed p.hands)
        |> List.length
        |> (==) 1


protagonistsHandsPlayedNumber : List Protagonist -> Int
protagonistsHandsPlayedNumber list =
    list
        |> List.filter (\p -> Hand.isProtagonistHandsPlayed p.hands)
        |> List.length


isProtagonistsHandsPlayed : List Protagonist -> Bool
isProtagonistsHandsPlayed list =
    list
        |> protagonistsHandsPlayedNumber
        |> (==) 3


isProtagonist : String -> List Protagonist -> Bool
isProtagonist s list =
    list
        |> getUserProtagonists s
        |> List.length
        |> (>) 0


getProtagonistFromNumber : Int -> List Protagonist -> Maybe Protagonist
getProtagonistFromNumber i list =
    list
        |> List.filter (\p -> p.number == i)
        |> List.head


getUserProtagonists : String -> List Protagonist -> List Protagonist
getUserProtagonists s list =
    List.filter (\p -> p.twitterScreenName == s) list


isLeader : String -> List Protagonist -> Bool
isLeader s list =
    case List.head list of
        Just p ->
            s == p.twitterScreenName

        Nothing ->
            False


isTurnProtagonist : String -> List Protagonist -> Bool
isTurnProtagonist s list =
    getTurnProtagonist s list
        |> ExMaybe.isJust


getTurnProtagonist : String -> List Protagonist -> Maybe Protagonist
getTurnProtagonist s list =
    turnProtagonist list
        |> Maybe.andThen
            (\p ->
                if p.twitterScreenName == s then
                    Just p

                else
                    Nothing
            )


turnProtagonistNumber : List Protagonist -> Maybe Int
turnProtagonistNumber list =
    turnProtagonist list
        |> Maybe.map .number


turnProtagonist : List Protagonist -> Maybe Protagonist
turnProtagonist list =
    list |> List.filter (\p -> not <| Hand.isProtagonistHandsPlayed p.hands) |> List.head


getSelectedHandComponentKey : Protagonist -> String
getSelectedHandComponentKey { number, hands } =
    Hand.getSelectedHandComponentKey number hands


getPlayedProtagonistHand : List Protagonist -> Maybe Hand
getPlayedProtagonistHand list =
    list
        |> getPlayedProtagonistsHands
        |> List.reverse
        |> List.head


getPlayedProtagonistsHands : List Protagonist -> List Hand
getPlayedProtagonistsHands list =
    list
        |> List.map (\p -> Hand.protagonistHandsPlayed p.hands)
        |> List.concat


getProtagonistCardUrl : Int -> String
getProtagonistCardUrl i =
    "/assets/images/hands/protagonist" ++ String.fromInt i ++ ".png"



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


handsForm : Protagonist -> (String -> msg) -> Html msg
handsForm { number, hands } chgMsg =
    let
        key =
            case Hand.getSelectedHand number hands of
                Just h ->
                    h.id

                Nothing ->
                    ""

        optionList =
            Hand.getFormOptionList number hands
    in
    Form.select ("form-protagonist-" ++ String.fromInt number ++ "-hand") chgMsg key optionList


selectedCard : Protagonist -> Html msg
selectedCard { number, hands } =
    case Hand.getSelectedHand number hands of
        Just h ->
            img [ src <| Hand.toCardUrl h ] []

        Nothing ->
            img [ src "/assets/images/hands/Unselected.png" ] []


selectedComponentCard : Protagonist -> Html msg
selectedComponentCard { number, hands } =
    case Hand.getSelectedHand number hands of
        Just h ->
            img [ src <| Hand.toComponentCardUrl h ] []

        Nothing ->
            img [ src "/assets/images/hands/Unselected.png" ] []


protagonistImg : Protagonist -> Html msg
protagonistImg p =
    img [ src <| getProtagonistCardUrl p.number, style "position" "absolute" ] []


usedCards : (Int -> Hand -> msg) -> Bool -> Protagonist -> Html msg
usedCards returnMsg isLeaderP p =
    div [ style "display" "flex" ] <|
        (if isLeaderP then
            div [ style "position" "relative", style "width" "35px" ]
                [ protagonistImg p
                , img [ src "/assets/images/others/leader.png", style "position" "absolute" ] []
                ]

         else
            div [ style "position" "relative", style "width" "35px" ] [ protagonistImg p ]
        )
            :: List.map (\h -> img [ src <| Hand.toCardUrl h, style "height" "50px", onClick (returnMsg p.number h) ] [])
                (Hand.usedHands p.hands)


useCardView : (Int -> Hand -> msg) -> List Protagonist -> List (Html msg)
useCardView returnMsg list =
    List.map
        (\p ->
            case List.head list of
                Just jp ->
                    usedCards returnMsg (jp == p) p

                Nothing ->
                    usedCards returnMsg False p
        )
        list
