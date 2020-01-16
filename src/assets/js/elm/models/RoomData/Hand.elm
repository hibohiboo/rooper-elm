module Models.RoomData.Hand exposing (..)

import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Json.Encode.Extra as ExEncode
import List.Extra as ExList
import Models.Board as Board exposing (BoardType)
import Models.Character exposing (CharacterType)
import Models.TragedySet as TragedySet exposing (Role)
import Models.Utility.List as UtilityList


type alias Hand =
    { handType : HandType
    , onCharacter : Maybe CharacterType
    , onBoard : Maybe BoardType
    , isUsed : Maybe Bool
    }


type HandType
    = ParanoiaPlus1
    | ParanoiaMinus1
    | ForbidParanoia
    | ForbidGoodwill
    | IntriguePlus1
    | IntriguePlus2
    | MovementVertical
    | MovementHorizontal
    | MovementDiagonal
    | GoodwillPlus1
    | GoodwillPlus2
    | ForbidIntrigue
    | ForbidMovement



-- ==============================================================================================
-- デコーダ
-- ==============================================================================================


decoder : D.Decoder Hand
decoder =
    D.succeed Hand
        |> Pipeline.required "handType" (D.map (typeFromString >> Maybe.withDefault ParanoiaPlus1) D.string)
        |> Pipeline.optional "onCharacter" (D.map Models.Character.characterTypeFromString D.string) Nothing
        |> Pipeline.optional "onBoard" (D.map Board.boardTypeFromString D.string) Nothing
        |> Pipeline.optional "isUsed" (D.maybe D.bool) Nothing



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : Hand -> E.Value
encode { handType, onCharacter, onBoard, isUsed } =
    E.object
        [ ( "handType", E.string <| typeToString handType )
        , ( "onCharacter", ExEncode.maybe (E.string << Models.Character.characterTypeToString) onCharacter )
        , ( "onBoard", ExEncode.maybe (E.string << Board.boardTypeToString) onBoard )
        , ( "isUsed", ExEncode.maybe E.bool isUsed )
        ]



-- ==============================================================================================
-- init
-- ==============================================================================================


initMastermind : List Hand
initMastermind =
    [ Hand ParanoiaPlus1 Nothing Nothing Nothing
    , Hand ParanoiaPlus1 Nothing Nothing Nothing
    , Hand ParanoiaMinus1 Nothing Nothing Nothing
    , Hand ForbidParanoia Nothing Nothing Nothing
    , Hand ForbidGoodwill Nothing Nothing Nothing
    , Hand IntriguePlus1 Nothing Nothing Nothing
    , Hand IntriguePlus2 Nothing Nothing (Just False)
    , Hand MovementVertical Nothing Nothing Nothing
    , Hand MovementHorizontal Nothing Nothing Nothing
    , Hand MovementDiagonal Nothing Nothing (Just False)
    ]


initProtagonist : List Hand
initProtagonist =
    [ Hand ParanoiaPlus1 Nothing Nothing Nothing
    , Hand ParanoiaMinus1 Nothing Nothing (Just False)
    , Hand GoodwillPlus1 Nothing Nothing Nothing
    , Hand GoodwillPlus2 Nothing Nothing (Just False)
    , Hand ForbidIntrigue Nothing Nothing Nothing
    , Hand MovementVertical Nothing Nothing Nothing
    , Hand MovementHorizontal Nothing Nothing Nothing
    , Hand ForbidMovement Nothing Nothing (Just False)
    ]



-- ==============================================================================================
-- メソッド
-- ==============================================================================================


toName : Hand -> String
toName hand =
    case hand.handType of
        ParanoiaPlus1 ->
            "不安+1"

        ParanoiaMinus1 ->
            "不安-1"

        ForbidParanoia ->
            "不安禁止"

        ForbidGoodwill ->
            "友好禁止"

        IntriguePlus1 ->
            "暗躍+1"

        IntriguePlus2 ->
            "暗躍+2"

        MovementVertical ->
            "移動上下"

        MovementHorizontal ->
            "移動左右"

        MovementDiagonal ->
            "移動斜め"

        GoodwillPlus1 ->
            "友好+1"

        GoodwillPlus2 ->
            "友好+2"

        ForbidIntrigue ->
            "暗躍禁止"

        ForbidMovement ->
            "移動禁止"


typeToString : HandType -> String
typeToString t =
    case t of
        ParanoiaPlus1 ->
            "ParanoiaPlus1"

        ParanoiaMinus1 ->
            "ParanoiaMinus1"

        ForbidParanoia ->
            "ForbidParanoia"

        ForbidGoodwill ->
            "ForbidGoodwill"

        IntriguePlus1 ->
            "IntriguePlus1"

        IntriguePlus2 ->
            "IntriguePlus2"

        MovementVertical ->
            "MovementVertical"

        MovementHorizontal ->
            "MovementHorizontal"

        MovementDiagonal ->
            "MovementDiagonal"

        GoodwillPlus1 ->
            "GoodwillPlus1"

        GoodwillPlus2 ->
            "GoodwillPlus2"

        ForbidIntrigue ->
            "ForbidIntrigue"

        ForbidMovement ->
            "ForbidMovement"


typeFromString : String -> Maybe HandType
typeFromString s =
    case s of
        "ParanoiaPlus1" ->
            Just ParanoiaPlus1

        "ParanoiaMinus1" ->
            Just ParanoiaMinus1

        "ForbidParanoia" ->
            Just ForbidParanoia

        "ForbidGoodwill" ->
            Just ForbidGoodwill

        "IntriguePlus1" ->
            Just IntriguePlus1

        "IntriguePlus2" ->
            Just IntriguePlus2

        "MovementVertical" ->
            Just MovementVertical

        "MovementHorizontal" ->
            Just MovementHorizontal

        "MovementDiagonal" ->
            Just MovementDiagonal

        "GoodwillPlus1" ->
            Just GoodwillPlus1

        "GoodwillPlus2" ->
            Just GoodwillPlus2

        "ForbidIntrigue" ->
            Just ForbidIntrigue

        "ForbidMovement" ->
            Just ForbidMovement

        _ ->
            Nothing
