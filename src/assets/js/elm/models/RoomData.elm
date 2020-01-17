module Models.RoomData exposing (..)

import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Json.Encode.Extra as ExEncode
import Models.Room as Room exposing (Room)
import Models.RoomData.Board as Board exposing (Board)
import Models.RoomData.Character as Character exposing (Character)
import Models.RoomData.Hand as Hand
import Models.RoomData.MasterMind as MasterMind exposing (MasterMind)
import Models.RoomData.OpenSheet as OpenSheet exposing (OpenSheet)
import Models.RoomData.Protagonist as Protagonist exposing (Protagonist)
import Models.RoomData.RoomDataState as RoomDataState exposing (RoomDataState)
import Models.Script as Script exposing (Script)
import Models.User exposing (User)


type alias RoomData =
    { id : String
    , mastermind : MasterMind
    , protagonists : List Protagonist
    , openSheet : OpenSheet
    , script : Maybe Script
    , loop : Int
    , date : Int
    , ex : Int
    , state : RoomDataState
    , characters : List Character
    , boards : List Board
    }


init : Maybe RoomData
init =
    Nothing


initRoomData : Room -> RoomData
initRoomData room =
    let
        mastermind =
            MasterMind.init room.mastermindTwitterScreenName

        protagonists =
            Protagonist.init room.protagonist1TwitterScreenName room.protagonist2TwitterScreenName room.protagonist3TwitterScreenName
    in
    RoomData (Room.getId room) mastermind protagonists (Script.scriptToOpenSheet room.script) Nothing 1 1 0 RoomDataState.init (Character.charactersFromCharacterScriptDataList room.script.characters) Board.init



-- Method


isRoomOwner : Maybe RoomData -> Bool
isRoomOwner room =
    case room of
        Just _ ->
            True

        Nothing ->
            False


isRoomMember : RoomData -> User -> Bool
isRoomMember data user =
    Protagonist.isProtagonist user.twitterScreenName data.protagonists



-- ==============================================================================================
-- setter
-- ==============================================================================================


setScript : Maybe Script -> RoomData -> RoomData
setScript s f =
    { f | script = s }


setLoop : String -> RoomData -> RoomData
setLoop s f =
    { f | loop = Maybe.withDefault 0 <| String.toInt s }


setEx : String -> RoomData -> RoomData
setEx s f =
    { f | ex = Maybe.withDefault 0 <| String.toInt s }


nextRoomDataState : RoomData -> RoomData
nextRoomDataState f =
    { f | state = RoomDataState.nextState f.state }


changeCharacterLocation : Character -> String -> RoomData -> RoomData
changeCharacterLocation c s f =
    { f
        | characters =
            List.map
                (\char ->
                    if char == c then
                        Character.setLocation s char

                    else
                        char
                )
                f.characters
    }


changeCharacterParameter : (Int -> Character -> Character) -> String -> Character -> Character -> Character
changeCharacterParameter method val c1 c2 =
    if c1 == c2 then
        method (Maybe.withDefault 0 <| String.toInt val) c1

    else
        c1


charangeCharactersParameter : (Int -> Character -> Character) -> Character -> String -> List Character -> List Character
charangeCharactersParameter method c s list =
    List.map (\char -> changeCharacterParameter method s char c) list


changeCharacterGoodWill : Character -> String -> RoomData -> RoomData
changeCharacterGoodWill c s f =
    { f | characters = charangeCharactersParameter Character.setGoodWill c s f.characters }


changeCharacterParanoia : Character -> String -> RoomData -> RoomData
changeCharacterParanoia c s f =
    { f | characters = charangeCharactersParameter Character.setParanoia c s f.characters }


changeCharacterIntrigue : Character -> String -> RoomData -> RoomData
changeCharacterIntrigue c s f =
    { f | characters = charangeCharactersParameter Character.setIntrigue c s f.characters }


toggleCharacterIsDead : Character -> RoomData -> RoomData
toggleCharacterIsDead c f =
    { f
        | characters =
            List.map
                (\char ->
                    if char == c then
                        Character.setIsDead (not char.isDead) char

                    else
                        char
                )
                f.characters
    }


deleteForbiddenLocation : Character -> RoomData -> RoomData
deleteForbiddenLocation c f =
    { f
        | characters =
            List.map
                (\char ->
                    if char == c then
                        Character.setForbiddenLocations [] char

                    else
                        char
                )
                f.characters
    }


changeBoardIntrigue : Board -> String -> RoomData -> RoomData
changeBoardIntrigue b s f =
    { f
        | boards =
            List.map
                (\board ->
                    if board == b then
                        Board.setIntrigue (String.toInt s |> Maybe.withDefault 0) board

                    else
                        board
                )
                f.boards
    }


changeMasterMindHand : Int -> String -> RoomData -> RoomData
changeMasterMindHand i s f =
    { f | mastermind = MasterMind.changeMasterMindHand i s f.mastermind }


changeMasterMindComponent : Int -> String -> RoomData -> RoomData
changeMasterMindComponent i s f =
    { f | mastermind = MasterMind.changeMasterMindComponent i s f.mastermind }



-- ==============================================================================================
-- getter
-- ==============================================================================================


isMastermindPlaysCards : Maybe RoomData -> Bool
isMastermindPlaysCards data =
    case data of
        Just d ->
            d.state == RoomDataState.MastermindPlaysCards

        Nothing ->
            False


isRoomStateHand : Maybe RoomData -> Bool
isRoomStateHand data =
    data
        |> Maybe.map (\d -> d.state == RoomDataState.MastermindPlaysCards)
        |> Maybe.withDefault False


isMastermindHandsSelected : RoomData -> Bool
isMastermindHandsSelected d =
    Hand.isMastermindHandsSelected d.mastermind.hands && d.state == RoomDataState.MastermindPlaysCards



-- ==============================================================================================
-- デコーダ
-- ==============================================================================================


decode : D.Value -> Maybe RoomData
decode json =
    D.decodeValue decoder json
        |> Result.toMaybe


decoder : D.Decoder RoomData
decoder =
    D.succeed RoomData
        |> Pipeline.required "id" D.string
        |> Pipeline.required "mastermind" MasterMind.decoder
        |> Pipeline.required "protagonists" (D.list Protagonist.decoder)
        |> Pipeline.required "openSheet" OpenSheet.decoder
        |> Pipeline.optional "script" Script.scriptDecoder Nothing
        |> Pipeline.optional "loop" D.int 0
        |> Pipeline.optional "date" D.int 0
        |> Pipeline.optional "ex" D.int 0
        |> Pipeline.optional "state" RoomDataState.decoder RoomDataState.init
        |> Pipeline.optional "characters" (D.list Character.decoder) []
        |> Pipeline.optional "boards" (D.list Board.decoder) []



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : RoomData -> E.Value
encode data =
    E.object
        [ ( "id", E.string data.id )
        , ( "mastermind", MasterMind.encode data.mastermind )
        , ( "protagonists", E.list Protagonist.encode data.protagonists )
        , ( "openSheet", OpenSheet.encode data.openSheet )
        , ( "script", ExEncode.maybe Script.encode data.script )
        , ( "loop", E.int data.loop )
        , ( "date", E.int data.date )
        , ( "ex", E.int data.ex )
        , ( "state", RoomDataState.encode data.state )
        , ( "characters", E.list Character.encode data.characters )
        , ( "boards", E.list Board.encode data.boards )
        ]



-- ==============================================================================================
-- View
-- ==============================================================================================


openSheetView : RoomData -> Html msg
openSheetView data =
    data.openSheet
        |> OpenSheet.openSheetView


closeSheetView : RoomData -> Html msg
closeSheetView data =
    case data.script of
        Just script ->
            Script.closeSheet script

        Nothing ->
            text ""


tags : Maybe RoomData -> User -> Html msg
tags data user =
    case data of
        Nothing ->
            text ""

        Just d ->
            div []
                (List.concat
                    [ [ if MasterMind.isMasterMind user.twitterScreenName d.mastermind then
                            span [ class "tag is-danger" ] [ text "脚本家" ]

                        else
                            text ""
                      ]
                    , d.protagonists
                        |> Protagonist.getUserProtagonists user.twitterScreenName
                        |> List.map
                            (\p ->
                                span
                                    [ class <|
                                        if p.number == 1 then
                                            "tag is-info"

                                        else if p.number == 2 then
                                            "tag is-warning"

                                        else
                                            "tag is-success"
                                    ]
                                    [ text p.name ]
                            )
                    ]
                )


infos : RoomData -> Html msg
infos data =
    let
        { loop, date, openSheet, ex } =
            data
    in
    section [ class "section", style "padding-top" "0.5rem", style "padding-bottom" "0.5rem" ]
        [ table [ class "table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Loop" ]
                    , th [] [ text "Date" ]
                    , th [] [ text "事件" ]
                    , th [] [ text "Ex" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [] [ text <| String.fromInt loop ]
                    , td [] [ text <| String.fromInt date ]
                    , td [] [ OpenSheet.incidentIcon date openSheet.incidents ]
                    , td [] [ text <| String.fromInt ex ]
                    ]
                ]
            ]
        ]


stateView : RoomData -> Html msg
stateView data =
    div [ class "rooper-roomdata-state" ]
        [ div [] [ text <| RoomDataState.toName data.state ]
        ]


charactersForm : RoomData -> (Character -> String -> msg) -> (Character -> String -> msg) -> (Character -> String -> msg) -> (Character -> String -> msg) -> (Character -> msg) -> (Character -> msg) -> Html msg
charactersForm data changeLocationMsg changeGMsg changePMsg changeIMsg toggleIsDeadMsg deleteForbiddenLocationMsg =
    div [ class "rooper-characters-form" ]
        (data.characters
            |> List.reverse
            |> List.map
                (\c -> Character.charactersFormItem c (changeLocationMsg c) (changeGMsg c) (changePMsg c) (changeIMsg c) (toggleIsDeadMsg c) (deleteForbiddenLocationMsg c))
        )


boardsForm : RoomData -> (Board -> String -> msg) -> Html msg
boardsForm data changeIMsg =
    div [ class "rooper-boards-form" ]
        (data.boards
            |> List.map
                (\b -> Board.boardsFormItem b (changeIMsg b))
        )


roomBoard : RoomData -> Html msg
roomBoard data =
    div [ class "rooper-roomdata-main-board-wrapper" ]
        [ div [ class "rooper-roomdata-main-board" ]
            [ boardHospital data
            , boardShrine data
            , boardCity data
            , boardSchool data
            ]
        ]


boardHospital : RoomData -> Html msg
boardHospital data =
    div [ class "rooper-main-board-hospital" ] <|
        Board.boardCard (Board.getHospital data.boards) (Character.isTurfHospital data.characters)
            :: List.map (\c -> Character.characterCard c)
                (Character.getCharactersOnHospital data.characters)


boardCity : RoomData -> Html msg
boardCity data =
    div [ class "rooper-main-board-city" ] <|
        Board.boardCard (Board.getCity data.boards) (Character.isTurfCity data.characters)
            :: List.map (\c -> Character.characterCard c)
                (Character.getCharactersOnCity data.characters)


boardShrine : RoomData -> Html msg
boardShrine data =
    div [ class "rooper-main-board-shrine" ] <|
        Board.boardCard (Board.getShrine data.boards) (Character.isTurfShrine data.characters)
            :: List.map (\c -> Character.characterCard c)
                (Character.getCharactersOnShrine data.characters)


boardSchool : RoomData -> Html msg
boardSchool data =
    div [ class "rooper-main-board-school" ] <|
        Board.boardCard (Board.getSchool data.boards) (Character.isTurfSchool data.characters)
            :: List.map (\c -> Character.characterCard c)
                (Character.getCharactersOnSchool data.characters)


handsForm : Int -> RoomData -> (String -> msg) -> (String -> msg) -> Html msg
handsForm i d handChangeMsg componentChangeMsg =
    div []
        [ div [ style "display" "flex" ]
            [ MasterMind.selectedCard i d.mastermind
            , MasterMind.selectedComponentCard i d.mastermind
            ]
        , if d.state == RoomDataState.MastermindPlaysCards then
            div []
                [ div [ style "padding-bottom" "20px" ] [ MasterMind.handsForm i d.mastermind handChangeMsg ]
                , handsOnComponentForm i d componentChangeMsg
                ]

          else
            text ""
        ]


handsOnComponentForm : Int -> RoomData -> (String -> msg) -> Html msg
handsOnComponentForm i data chgMsg =
    let
        key =
            MasterMind.getSelectedHandComponentKey i data.mastermind

        optionList =
            ( "未選択", "未選択" )
                :: List.concat
                    [ Board.getFormOptionList (Hand.getSelectedBoardComponentType i data.mastermind.hands) data.boards
                    , Character.getFormOptionList (Hand.getSelectedCharacterComponentType i data.mastermind.hands) data.characters
                    ]
    in
    Form.select ("form-mastermind-hand-" ++ String.fromInt i) chgMsg key optionList
