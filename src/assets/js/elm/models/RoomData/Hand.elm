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
import Models.Character as Character exposing (CharacterType)
import Models.TragedySet as TragedySet exposing (Role)
import Models.Utility.List as UtilityList


type ComponentType
    = CharacterComponentType CharacterType
    | BoardComponentType BoardType


type alias Hand =
    { id : String
    , formId : Int -- 選択された手札の番号
    , handType : HandType
    , isSelected : Bool
    , onComponent : Maybe ComponentType
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
        |> Pipeline.required "id" D.string
        |> Pipeline.required "formId" D.int
        |> Pipeline.required "handType" (D.map (typeFromString >> Maybe.withDefault ParanoiaPlus1) D.string)
        |> Pipeline.required "isSelected" D.bool
        |> Pipeline.optional "onComponent" (D.map componentTypeFromString D.string) Nothing
        |> Pipeline.optional "isUsed" (D.maybe D.bool) Nothing



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : Hand -> E.Value
encode { id, formId, handType, isSelected, onComponent, isUsed } =
    E.object
        [ ( "id", E.string id )
        , ( "formId", E.int formId )
        , ( "handType", E.string <| typeToString handType )
        , ( "isSelected", E.bool isSelected )
        , ( "onComponent", ExEncode.maybe (E.string << componentTypeToString) onComponent )
        , ( "isUsed", ExEncode.maybe E.bool isUsed )
        ]



-- ==============================================================================================
-- init
-- ==============================================================================================


initMastermind : List Hand
initMastermind =
    [ Hand "m0" 1 ParanoiaPlus1 True Nothing Nothing
    , Hand "m1" 2 ParanoiaPlus1 True Nothing Nothing
    , Hand "m2" 3 ParanoiaMinus1 True Nothing Nothing
    , Hand "m3" 0 ForbidParanoia False Nothing Nothing
    , Hand "m4" 0 ForbidGoodwill False Nothing Nothing
    , Hand "m5" 0 IntriguePlus1 False Nothing Nothing
    , Hand "m6" 0 IntriguePlus2 False Nothing (Just False)
    , Hand "m7" 0 MovementVertical False Nothing Nothing
    , Hand "m8" 0 MovementHorizontal False Nothing Nothing
    , Hand "m9" 0 MovementDiagonal False Nothing (Just False)
    ]


initProtagonist : List Hand
initProtagonist =
    [ Hand "p0" 1 ParanoiaPlus1 True Nothing Nothing
    , Hand "p1" 0 ParanoiaMinus1 False Nothing (Just False)
    , Hand "p2" 0 GoodwillPlus1 False Nothing Nothing
    , Hand "p3" 0 GoodwillPlus2 False Nothing (Just False)
    , Hand "p4" 0 ForbidIntrigue False Nothing Nothing
    , Hand "p5" 0 MovementVertical False Nothing Nothing
    , Hand "p6" 0 MovementHorizontal False Nothing Nothing
    , Hand "p7" 0 ForbidMovement False Nothing (Just False)
    ]



-- ==============================================================================================
-- メソッド
-- ==============================================================================================
-- ==============================================================================================
-- setter
-- ==============================================================================================


changeMasterMindHand : Int -> String -> List Hand -> List Hand
changeMasterMindHand i s list =
    list
        |> List.map
            (\h ->
                if h.formId == i then
                    { h | formId = 0, onComponent = Nothing }

                else if h.id == s then
                    { h | formId = i }

                else
                    h
            )


changeMasterMindComponent : Int -> String -> List Hand -> List Hand
changeMasterMindComponent i s list =
    list
        |> List.map
            (\h ->
                if h.formId == i then
                    { h | onComponent = componentTypeFromString s }

                else
                    h
            )



-- ==============================================================================================
-- getter
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


toCardUrl : Hand -> String
toCardUrl h =
    "/assets/images/hands/"
        ++ (case h.id of
                "p1" ->
                    "p1"

                _ ->
                    typeToString h.handType
           )
        ++ ".png"


toComponentCardUrl : Hand -> String
toComponentCardUrl h =
    case h.onComponent of
        Just (BoardComponentType b) ->
            Board.boardToCardUrl b

        Just (CharacterComponentType c) ->
            Character.characterTypeToCardUrl c

        Nothing ->
            "/assets/images/hands/Unselected.png"


componentTypeFromString : String -> Maybe ComponentType
componentTypeFromString s =
    case Character.characterTypeFromString s of
        Just ct ->
            Just (CharacterComponentType ct)

        Nothing ->
            case Board.boardTypeFromString s of
                Just bt ->
                    Just (BoardComponentType bt)

                Nothing ->
                    Nothing


componentTypeToString : ComponentType -> String
componentTypeToString t =
    case t of
        CharacterComponentType ct ->
            Character.characterTypeToString ct

        BoardComponentType bt ->
            Board.boardTypeToString bt


getSelectedHand : Int -> List Hand -> Maybe Hand
getSelectedHand i list =
    list
        |> List.filter (\h -> h.formId == i)
        |> List.head


getSelectedHandComponentKey : Int -> List Hand -> String
getSelectedHandComponentKey i list =
    case getSelectedHand i list of
        Just h ->
            case h.onComponent of
                Just c ->
                    componentTypeToString c

                Nothing ->
                    "未選択"

        Nothing ->
            "未選択"


getFormHandList : Int -> List Hand -> List Hand
getFormHandList i list =
    list
        |> List.filter (\h -> h.formId == i || h.formId == 0)


getFormOptionList : Int -> List Hand -> List ( String, String )
getFormOptionList i list =
    getFormHandList i list
        |> List.map (\h -> ( h.id, toName h ))
