module Models.RoomData.Character exposing (..)

import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Json.Encode.Extra as ExEncode
import Models.Board as Board exposing (Board)
import Models.Character exposing (CharacterScriptData, CharacterType)
import Models.TragedySet as TragedySet exposing (Role)
import Models.Utility.List as UtilityList


type alias Character =
    { characterType : CharacterType
    , name : String
    , paranoiaLimit : Int
    , firstLocation : Board
    , role : Maybe Role -- 役職
    , optionalNumber : Maybe Int -- 神格の場合、登場ループ数。転校生の場合、登場日数。
    , turf : Maybe Board -- 大物の場合のテリトリー
    , goodWill : Int -- 友好
    , paranoia : Int -- 不安
    , intrigue : Int -- 暗躍
    , location : Maybe Board -- 現在のボード
    , forbiddenLocations : List Board -- 禁止エリア
    }



-- ==============================================================================================
-- init
-- ==============================================================================================


charactersFromCharacterScriptDataList : List CharacterScriptData -> List Character
charactersFromCharacterScriptDataList list =
    List.map characterFromCharacterScriptData list


characterFromCharacterScriptData : CharacterScriptData -> Character
characterFromCharacterScriptData { character, role, optionalNumber, turf } =
    let
        { characterType, name, paranoiaLimit, firstLocation, forbiddenLocations } =
            character
    in
    Character characterType name paranoiaLimit firstLocation role optionalNumber turf 0 0 0 (Just firstLocation) forbiddenLocations



-- ==============================================================================================
-- メソッド
-- ==============================================================================================
-- ==============================================================================================
-- setter
-- ==============================================================================================


setFirstLocation : Board -> Character -> Character
setFirstLocation b c =
    { c | firstLocation = b }


setLocation : String -> Character -> Character
setLocation s c =
    { c | location = Board.boardFromString s }


setGoodWill : Int -> Character -> Character
setGoodWill v c =
    { c | goodWill = v }


setParanoia : Int -> Character -> Character
setParanoia v c =
    { c | paranoia = v }


setIntrigue : Int -> Character -> Character
setIntrigue v c =
    { c | intrigue = v }



-- ==============================================================================================
-- デコーダ
-- ==============================================================================================


decoder : D.Decoder Character
decoder =
    D.succeed Character
        |> Pipeline.required "characterType" (D.map (Models.Character.characterTypeFromString >> Maybe.withDefault Models.Character.BoyStudent) D.string)
        |> Pipeline.required "name" D.string
        |> Pipeline.required "paranoiaLimit" D.int
        |> Pipeline.required "firstLocation" Board.decode
        |> Pipeline.optional "role" TragedySet.decodeRole Nothing
        |> Pipeline.optional "optionalNumber" (D.maybe D.int) Nothing
        |> Pipeline.optional "turf" Board.decodeBoard Nothing
        |> Pipeline.required "goodWill" D.int
        |> Pipeline.required "paranoia" D.int
        |> Pipeline.required "intrigue" D.int
        |> Pipeline.optional "location" Board.decodeBoard Nothing
        |> Pipeline.required "forbiddenLocations" (D.list Board.decode)



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : Character -> E.Value
encode { characterType, name, paranoiaLimit, firstLocation, role, optionalNumber, turf, goodWill, paranoia, intrigue, location, forbiddenLocations } =
    E.object
        [ ( "name", E.string name )
        , ( "characterType", E.string <| Models.Character.characterTypeToString characterType )
        , ( "paranoiaLimit", E.int paranoiaLimit )
        , ( "firstLocation", E.string <| Board.boardToString firstLocation )
        , ( "role", ExEncode.maybe E.string <| Maybe.map TragedySet.roleToString role )
        , ( "optionalNumber", ExEncode.maybe E.int optionalNumber )
        , ( "turf", ExEncode.maybe E.string <| Maybe.map Board.boardToString turf )
        , ( "goodWill", E.int goodWill )
        , ( "paranoia", E.int paranoia )
        , ( "intrigue", E.int intrigue )
        , ( "location", ExEncode.maybe E.string <| Maybe.map Board.boardToString location )
        , ( "forbiddenLocations", E.list (E.string << Board.boardToString) forbiddenLocations )
        ]



-- ==============================================================================================
-- Method
-- ==============================================================================================


toString : Character -> String
toString c =
    Models.Character.characterTypeToString c.characterType


characterToCardUrl : Character -> String
characterToCardUrl c =
    Models.Character.characterTypeToCardUrl c.characterType


boardToName : Maybe Board -> String
boardToName b =
    case b of
        Just board ->
            board.name

        Nothing ->
            ""


boardList : Character -> List ( String, String )
boardList c =
    List.map (\b -> Tuple.pair (Board.boardToString b) b.name) (UtilityList.exceptList c.forbiddenLocations Board.boards)


boardListWithNothing : Character -> List ( String, String )
boardListWithNothing c =
    ( "ボード外", "ボード外" ) :: boardList c



-- ==============================================================================================
-- View
-- ==============================================================================================


charactersFormItem : Character -> (String -> msg) -> (String -> msg) -> (String -> msg) -> (String -> msg) -> Html msg
charactersFormItem c changeLocationMsg changeGMsg changePMsg changeIMsg =
    div []
        [ div [ class "rooper-character-room-form-item" ]
            [ img [ src (characterToCardUrl c) ] []
            , div []
                [ text "ボード"
                , div [] [ characterLocationBoards c changeLocationMsg ]
                ]
            , div []
                [ text "友好"
                , div []
                    [ input [ value <| String.fromInt c.goodWill, onChange changeGMsg, type_ "number" ] []
                    ]
                ]
            , div []
                [ text "不安"
                , div []
                    [ input [ value <| String.fromInt c.paranoia, onChange changePMsg, type_ "number" ] []
                    ]
                ]
            , div []
                [ text "暗躍"
                , div []
                    [ input [ value <| String.fromInt c.intrigue, onChange changeIMsg, type_ "number" ] []
                    ]
                ]
            , div []
                [ text "死"
                , div [] [ input [ type_ "checkbox" ] [] ]
                ]
            ]
        , div [] [ text c.name ]
        ]


characterLocationBoards : Character -> (String -> msg) -> Html msg
characterLocationBoards char chgMsg =
    let
        boardKey =
            case char.location of
                Just b ->
                    Board.boardToString b

                Nothing ->
                    "ボード外"

        optionList =
            case char.characterType of
                Models.Character.Illusion ->
                    boardListWithNothing char

                Models.Character.GodlyBeing ->
                    boardListWithNothing char

                Models.Character.TransferStudent ->
                    boardListWithNothing char

                _ ->
                    boardList char
    in
    Form.select ("bottom-form-character-board-" ++ toString char) chgMsg boardKey optionList
