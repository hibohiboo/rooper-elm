module Models.RoomData exposing (..)

import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Extra as ExHtml
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Json.Encode.Extra as ExEncode
import Models.Room as Room exposing (Room)
import Models.RoomData.Board as Board exposing (Board)
import Models.RoomData.Character as Character exposing (Character)
import Models.RoomData.Hand as Hand exposing (Hand)
import Models.RoomData.MasterMind as MasterMind exposing (MasterMind)
import Models.RoomData.OpenSheet as OpenSheet exposing (OpenSheet)
import Models.RoomData.Protagonist as Protagonist exposing (Protagonist)
import Models.RoomData.RoomDataState as RoomDataState exposing (RoomDataState)
import Models.Script as Script exposing (Script)
import Models.TragedySet as TragedySet
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


initDefault : RoomData
initDefault =
    let
        mastermind =
            MasterMind.init "forTestRoom"

        protagonists =
            Protagonist.init "hoge1" "hoge2" "hoge3"

        script =
            Script.initDefault
    in
    RoomData "roomTest" mastermind protagonists (Script.scriptToOpenSheet script) Nothing 0 0 0 RoomDataState.init (Character.charactersFromCharacterScriptDataList script.characters) Board.init


initRoomData : Room -> RoomData
initRoomData room =
    let
        mastermind =
            MasterMind.init room.mastermindTwitterScreenName

        protagonists =
            Protagonist.init room.protagonist1TwitterScreenName room.protagonist2TwitterScreenName room.protagonist3TwitterScreenName
    in
    RoomData (Room.getId room) mastermind protagonists (Script.scriptToOpenSheet room.script) Nothing 0 0 0 RoomDataState.init (Character.charactersFromCharacterScriptDataList room.script.characters) Board.init



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


loopEnd : RoomData -> RoomData
loopEnd data =
    { data | state = RoomDataState.TimeSpairal }


nextRoomDataState : RoomData -> RoomData
nextRoomDataState data =
    data
        |> updateNextState
        |> updateHandsData
        |> updateProtagonists
        |> updateDate
        |> updateLoop
        |> updateBoardsCharacters
        |> updateHands


updateNextState : RoomData -> RoomData
updateNextState f =
    if f.state == RoomDataState.ProtagonistsPlaysCard && not (isProtagonistsPlayed f) then
        -- 主人公がカードを置き終わるまで、主人公行動フェイズは更新されない
        f

    else if f.state == RoomDataState.Night && f.openSheet.daysInOneLoop == f.date then
        -- 最終日のターン終了フェイズの次は時のはざまとする
        { f | state = RoomDataState.TimeSpairal }

    else
        { f | state = RoomDataState.nextState f.state }


updateHandsData : RoomData -> RoomData
updateHandsData data =
    if data.state == RoomDataState.MastemindAbilities then
        { data | mastermind = MasterMind.returnPlayedHands data.mastermind, protagonists = Protagonist.returnPlayedHands data.protagonists }

    else
        data


updateProtagonists : RoomData -> RoomData
updateProtagonists data =
    if data.state == RoomDataState.Night then
        { data | protagonists = Protagonist.changeLeader data.protagonists }

    else
        data


updateDate : RoomData -> RoomData
updateDate data =
    if data.state == RoomDataState.Morning then
        { data | date = data.date + 1 }

    else
        data


updateLoop : RoomData -> RoomData
updateLoop data =
    if data.state == RoomDataState.SetupCharacter then
        { data | loop = data.loop + 1, date = 0 }

    else
        data


updateBoardsCharacters : RoomData -> RoomData
updateBoardsCharacters data =
    if data.state == RoomDataState.SetupCounter then
        { data | characters = Character.resetCharacters data.characters, boards = Board.init }

    else
        data


updateHands : RoomData -> RoomData
updateHands data =
    if data.state == RoomDataState.SetupHand then
        { data | mastermind = MasterMind.init data.mastermind.twitterScreenName, protagonists = Protagonist.reset data.protagonists }

    else
        data


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


toggleCharacterIsSetEx : Character -> RoomData -> RoomData
toggleCharacterIsSetEx c f =
    { f
        | characters =
            List.map
                (\char ->
                    if char == c then
                        Character.setIsSetEx (not char.isSetEx) char

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


changeProtagonistHand : Int -> String -> RoomData -> RoomData
changeProtagonistHand i s f =
    { f | protagonists = Protagonist.changeProtagonistsHand i s f.protagonists }


changeProtagonistComponent : Int -> String -> RoomData -> RoomData
changeProtagonistComponent i s f =
    { f | protagonists = Protagonist.changeProtagonistsComponent i s f.protagonists }


unusedProtagonistHand : Int -> Hand -> RoomData -> RoomData
unusedProtagonistHand i h f =
    { f | protagonists = Protagonist.unusedProtagonistHand i h f.protagonists }


resolveCards : RoomData -> RoomData
resolveCards data =
    let
        hands =
            playedHands data
    in
    data
        |> resolveCardOnBoards hands
        |> resolveCardOnCharacters hands


resolveCardOnBoards : List Hand -> RoomData -> RoomData
resolveCardOnBoards hands data =
    { data
        | boards =
            data.boards |> List.map (\b -> Board.resolveCard hands b)
    }


resolveCardOnCharacters : List Hand -> RoomData -> RoomData
resolveCardOnCharacters hands data =
    { data
        | characters =
            data.characters |> List.map (\c -> Character.resolveCard hands c)
    }



-- ==============================================================================================
-- getter
-- ==============================================================================================


isDisplayMastermindBottomForm : User -> RoomData -> Bool
isDisplayMastermindBottomForm user data =
    case data.state of
        RoomDataState.ProtagonistsPlaysCard ->
            isProtagonistsPlayed data

        _ ->
            user.twitterScreenName == data.mastermind.twitterScreenName


isMastermindPlaysCards : RoomData -> Bool
isMastermindPlaysCards d =
    d.state == RoomDataState.MastermindPlaysCards


isProtagonistsPlaysCard : RoomData -> Bool
isProtagonistsPlaysCard d =
    d.state == RoomDataState.ProtagonistsPlaysCard


isRoomStateHand : Maybe RoomData -> Bool
isRoomStateHand data =
    data
        |> Maybe.map (\d -> d.state == RoomDataState.MastermindPlaysCards || d.state == RoomDataState.ProtagonistsPlaysCard)
        |> Maybe.withDefault False


isCardsAreResolvedState : Maybe RoomData -> Bool
isCardsAreResolvedState data =
    data
        |> Maybe.map (\d -> d.state == RoomDataState.CardsAreResolved)
        |> Maybe.withDefault False


isMastermindHandsPlayed : RoomData -> Bool
isMastermindHandsPlayed d =
    Hand.isMastermindHandsPlayed d.mastermind.hands
        && d.state
        == RoomDataState.MastermindPlaysCards
        || isProtagonistsPlayed d
        && d.state
        == RoomDataState.ProtagonistsPlaysCard


isProtagonistHandsPlayed : Int -> RoomData -> Bool
isProtagonistHandsPlayed i d =
    Protagonist.isProtagonistHandsPlayed i d.protagonists


getAppearedCharacters : RoomData -> List Character
getAppearedCharacters data =
    data.characters
        |> Character.filterTransferStudent data.date
        |> Character.filterGodlyBeing data.loop


isLeader : User -> RoomData -> Bool
isLeader user data =
    Protagonist.isLeader user.twitterScreenName data.protagonists


isTurnProtagonist : Int -> User -> RoomData -> Bool
isTurnProtagonist i user data =
    data.state
        == RoomDataState.ProtagonistsPlaysCard
        && (Protagonist.getProtagonistFromNumber i data.protagonists
                |> Maybe.map (\p -> p.twitterScreenName == user.twitterScreenName)
                |> Maybe.withDefault False
           )


getTurnProtagonistNumber : RoomData -> Int
getTurnProtagonistNumber { protagonists } =
    Protagonist.turnProtagonistNumber protagonists |> Maybe.withDefault 0


isProtagonistsPlayed : RoomData -> Bool
isProtagonistsPlayed data =
    Protagonist.isProtagonistsHandsPlayed data.protagonists


playedHands : RoomData -> List Hand
playedHands { mastermind, protagonists } =
    List.concat [ Protagonist.getPlayedProtagonistsHands protagonists, MasterMind.getPlayedHands mastermind ]



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
                    , case openSheet.set.setType of
                        TragedySet.BasicTragedy ->
                            ExHtml.nothing

                        TragedySet.FirstSteps ->
                            ExHtml.nothing

                        TragedySet.MysteryCircle ->
                            th [] [ text "Ex" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [] [ text <| String.fromInt loop ]
                    , td [] [ text <| String.fromInt date ]
                    , td [] [ OpenSheet.incidentIcon date openSheet.incidents ]
                    , case openSheet.set.setType of
                        TragedySet.BasicTragedy ->
                            ExHtml.nothing

                        TragedySet.FirstSteps ->
                            ExHtml.nothing

                        TragedySet.MysteryCircle ->
                            td [] [ text <| String.fromInt ex ]
                    ]
                ]
            ]
        ]


stateView : RoomData -> Html msg
stateView data =
    div [ class "rooper-roomdata-state" ]
        [ div [] [ text <| RoomDataState.toName data.state ]
        ]


charactersForm : RoomData -> (Character -> String -> msg) -> (Character -> String -> msg) -> (Character -> String -> msg) -> (Character -> String -> msg) -> (Character -> msg) -> (Character -> msg) -> (Character -> msg) -> Html msg
charactersForm data changeLocationMsg changeGMsg changePMsg changeIMsg toggleIsDeadMsg deleteForbiddenLocationMsg toggleIsSetExMsg =
    div [ class "rooper-characters-form" ]
        (data.characters
            |> List.reverse
            |> List.map
                (\c -> Character.charactersFormItem c (data.openSheet.set.setType == TragedySet.MysteryCircle) (changeLocationMsg c) (changeGMsg c) (changePMsg c) (changeIMsg c) (toggleIsDeadMsg c) (deleteForbiddenLocationMsg c) (toggleIsSetExMsg c))
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
        boardCard data (Board.getHospital data.boards) Character.isTurfHospital
            :: List.map (\c -> characterCard data c)
                (Character.getCharactersOnHospital <| getAppearedCharacters data)


boardCity : RoomData -> Html msg
boardCity data =
    div [ class "rooper-main-board-city" ] <|
        boardCard data (Board.getCity data.boards) Character.isTurfCity
            :: List.map (\c -> characterCard data c)
                (Character.getCharactersOnCity <| getAppearedCharacters data)


boardShrine : RoomData -> Html msg
boardShrine data =
    div [ class "rooper-main-board-shrine" ] <|
        boardCard data (Board.getShrine data.boards) Character.isTurfShrine
            :: List.map (\c -> characterCard data c)
                (Character.getCharactersOnShrine <| getAppearedCharacters data)


boardSchool : RoomData -> Html msg
boardSchool data =
    div [ class "rooper-main-board-school" ] <|
        boardCard data (Board.getSchool data.boards) Character.isTurfSchool
            :: List.map (\c -> characterCard data c)
                (Character.getCharactersOnSchool <| getAppearedCharacters data)


boardCard : RoomData -> Board -> (List Character -> Bool) -> Html msg
boardCard data board isTurf =
    div [ class "rooper-roomdata-board-card-wrapper" ] <|
        Board.boardCard board (isTurf data.characters)
            :: (if data.state == RoomDataState.CardsAreResolved then
                    []

                else
                    [ case Hand.getSelectedBoardHand board.boardType (Protagonist.getPlayedProtagonistsHands data.protagonists) of
                        Just h ->
                            img [ class "protagonist-hand", src <| Protagonist.getProtagonistCardUrl h.formId ] []

                        Nothing ->
                            text ""
                    , if Hand.isBoardSelected board.boardType data.mastermind.hands then
                        img [ class "mastermind-hand", src "/assets/images/hands/mastermind.png" ] []

                      else
                        text ""
                    ]
               )


characterCard : RoomData -> Character -> Html msg
characterCard data char =
    div [ class "rooper-roomdata-character-card-wrapper" ] <|
        Character.characterCard char
            :: (if data.state == RoomDataState.CardsAreResolved then
                    []

                else
                    [ case Hand.getSelectedCharacterHand char.characterType (Protagonist.getPlayedProtagonistsHands data.protagonists) of
                        Just h ->
                            img [ class "protagonist-hand", src <| Protagonist.getProtagonistCardUrl h.formId ] []

                        Nothing ->
                            text ""
                    , if Hand.isCharacterSelected char.characterType data.mastermind.hands then
                        img [ class "mastermind-hand", src "/assets/images/hands/mastermind.png" ] []

                      else
                        text ""
                    ]
               )


handsFormMastermind : Int -> RoomData -> (String -> msg) -> (String -> msg) -> Html msg
handsFormMastermind i d handChangeMsg componentChangeMsg =
    div []
        [ div [ style "display" "flex" ]
            [ MasterMind.selectedCard i d.mastermind
            , MasterMind.selectedComponentCard i d.mastermind
            ]
        , if d.state == RoomDataState.MastermindPlaysCards then
            div []
                [ div [ style "padding-bottom" "20px" ] [ MasterMind.handsForm i d.mastermind handChangeMsg ]
                , handsOnComponentFormMastermind i d componentChangeMsg
                ]

          else
            text ""
        ]


handsFormProtagonist : Int -> User -> RoomData -> (String -> msg) -> (String -> msg) -> Html msg
handsFormProtagonist i u d handChangeMsg componentChangeMsg =
    case Protagonist.getProtagonistFromNumber i d.protagonists of
        Just p ->
            div []
                [ div [ style "display" "flex" ]
                    [ Protagonist.selectedCard p
                    , Protagonist.selectedComponentCard p
                    ]
                , if d.state == RoomDataState.ProtagonistsPlaysCard then
                    div []
                        [ div [ style "padding-bottom" "20px" ] [ Protagonist.handsForm p handChangeMsg ]
                        , handsOnComponentFormProtagonist p d componentChangeMsg
                        ]

                  else
                    text ""
                ]

        Nothing ->
            text "異常ケース"


handsOnComponentFormMastermind : Int -> RoomData -> (String -> msg) -> Html msg
handsOnComponentFormMastermind i data chgMsg =
    let
        key =
            MasterMind.getSelectedHandComponentKey i data.mastermind

        optionList =
            ( "未選択", "未選択" )
                :: List.concat
                    [ Board.getFormOptionList (Hand.getSelectedBoardComponentType i data.mastermind.hands) data.boards
                    , getAppearedCharacters data
                        |> Character.getFormOptionList (Hand.getSelectedCharacterComponentType i data.mastermind.hands)
                    ]
    in
    Form.select ("form-mastermind-on-component-" ++ String.fromInt i) chgMsg key optionList


handsOnComponentFormProtagonist : Protagonist -> RoomData -> (String -> msg) -> Html msg
handsOnComponentFormProtagonist p data chgMsg =
    let
        key =
            Protagonist.getSelectedHandComponentKey p

        hands =
            Protagonist.getPlayedProtagonistsHands data.protagonists

        optionList =
            ( "未選択", "未選択" )
                :: List.concat
                    [ Board.getFormOptionList (Hand.getSelectedBoardComponentType p.number hands) data.boards
                    , getAppearedCharacters data
                        |> List.filter
                            (\c ->
                                if c.isSetEx && data.openSheet.set.setType == TragedySet.MysteryCircle then
                                    False

                                else
                                    True
                            )
                        |> Character.getFormOptionList (Hand.getSelectedCharacterComponentType p.number hands)
                    ]
    in
    Form.select ("form-protagonist-on-component-" ++ String.fromInt p.number) chgMsg key optionList


playedHandsView : RoomData -> Html msg
playedHandsView d =
    div [ style "display" "flex", style "justify-content" "space-evenly" ]
        [ div [] <|
            List.map
                (\i ->
                    div [ style "display" "flex" ]
                        [ img [ src "/assets/images/hands/mastermind.png" ] []
                        , if d.state == RoomDataState.CardsAreResolved then
                            MasterMind.selectedCard i d.mastermind

                          else
                            img [ src "/assets/images/hands/mastermind.png" ] []
                        , MasterMind.selectedComponentCard i d.mastermind
                        ]
                )
                (List.range 1 3)
        , div [] <|
            List.map
                (\p ->
                    div [ style "display" "flex" ]
                        [ img [ src <| Protagonist.getProtagonistCardUrl p.number ] []
                        , if d.state == RoomDataState.CardsAreResolved then
                            Protagonist.selectedCard p

                          else
                            img [ src <| Protagonist.getProtagonistCardUrl p.number ] []
                        , Protagonist.selectedComponentCard p
                        ]
                )
                d.protagonists
        ]


usedHands : (Int -> Hand -> msg) -> RoomData -> Html msg
usedHands returnMsg d =
    div [ style "display" "flex", style "justify-content" "space-evenly", style "flex-wrap" "wrap" ] <| List.concat [ Protagonist.useCardView returnMsg d.protagonists, [ MasterMind.useCardView d.mastermind ] ]


roomDataView : RoomData -> Html msg
roomDataView data =
    div [ class "box" ]
        [ boardsView data
        , charactersView data
        ]


boardsView : RoomData -> Html msg
boardsView data =
    div [ class "rooper-boards-form" ]
        (data.boards
            |> List.map
                (\b -> Board.boardsViewItem b)
        )


charactersView : RoomData -> Html msg
charactersView data =
    div [ class "rooper-characters-form" ]
        (getAppearedCharacters data
            |> List.reverse
            |> List.map
                (\c -> Character.charactersViewItem c)
        )


tweetView : RoomData -> User -> Int -> msg -> Html msg
tweetView data user turnNumber closeModelMsg =
    let
        componentText =
            if Protagonist.protagonistsHandsPlayedNumber data.protagonists == 0 then
                List.foldl (++) "" <|
                    List.map (\h -> "【" ++ Hand.handToComponentName h ++ "】") <|
                        MasterMind.getPlayedHands data.mastermind

            else
                -- 主人公の手番がまだあるかを確認
                case Protagonist.turnProtagonist data.protagonists of
                    Just _ ->
                        -- あれば、選択済のカードの置き場所を取得
                        case Protagonist.getPlayedProtagonistHand data.protagonists of
                            Just h ->
                                "【" ++ Hand.handToComponentName h ++ "】"

                            Nothing ->
                                "ここはこないはず"

                    Nothing ->
                        case Protagonist.getPlayedProtagonistHand data.protagonists of
                            Just h ->
                                "【" ++ Hand.handToComponentName h ++ "】"

                            Nothing ->
                                "ここはこないはず"

        tweetText =
            if Protagonist.protagonistsHandsPlayedNumber data.protagonists == 0 then
                "脚本家が" ++ componentText ++ "に手札をセットしました。"

            else
                case Protagonist.getProtagonistFromNumber turnNumber data.protagonists of
                    Just p ->
                        "主人公" ++ String.fromInt p.number ++ "が" ++ componentText ++ "に手札をセットしました。"

                    Nothing ->
                        "ここにはこない"

        nextUser =
            case Protagonist.turnProtagonist data.protagonists of
                Just p ->
                    p.twitterScreenName

                Nothing ->
                    data.mastermind.twitterScreenName

        url =
            "https://twitter.com/intent/tweet?screen_name="
                ++ nextUser
                ++ "&button_hashtag=惨劇オンライン&text="
                ++ tweetText
                ++ "&ref_src=twsrc%5Etfw"
    in
    div []
        [ div [ class "columns is-mobile" ]
            [ text <| componentText ++ "に手札をセットしました。"
            ]
        , div [ class "columns is-mobile" ]
            [ div [ class "column  is-4  is-offset-1 control" ]
                [ a
                    [ class "button is-primary twitter-hashtag-button"
                    , attribute "data-text" "test"
                    , href url
                    ]
                    [ span [ class "icon" ] [ i [ class "fab fa-twitter" ] [] ]
                    , span [] [ text "呟く" ]
                    ]
                ]
            , div [ class "column  is-4 is-offset-2 control" ]
                [ button [ class "button is-info", onClick closeModelMsg ] [ text "閉じる" ] ]
            ]
        ]
