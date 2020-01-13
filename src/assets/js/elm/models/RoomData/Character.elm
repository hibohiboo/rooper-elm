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
-- View
-- ==============================================================================================
