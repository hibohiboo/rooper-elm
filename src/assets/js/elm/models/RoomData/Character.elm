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
import List.Extra as ExList
import Models.Board as Board exposing (Board, BoardType(..))
import Models.Character exposing (CharacterScriptData, CharacterType)
import Models.RoomData.Hand as Hand exposing (Hand, HandType(..))
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
    , isDead : Bool -- 死亡
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
    Character characterType name paranoiaLimit firstLocation role optionalNumber turf 0 0 0 (Just firstLocation) forbiddenLocations False



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
        |> Pipeline.optional "isDead" D.bool False



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : Character -> E.Value
encode { characterType, name, paranoiaLimit, firstLocation, role, optionalNumber, turf, goodWill, paranoia, intrigue, location, forbiddenLocations, isDead } =
    E.object
        [ ( "name", E.string name )
        , ( "characterType", E.string <| Models.Character.characterTypeToString characterType )
        , ( "paranoiaLimit", E.int paranoiaLimit )
        , ( "firstLocation", E.string <| Board.toString firstLocation )
        , ( "role", ExEncode.maybe E.string <| Maybe.map TragedySet.roleToString role )
        , ( "optionalNumber", ExEncode.maybe E.int optionalNumber )
        , ( "turf", ExEncode.maybe E.string <| Maybe.map Board.toString turf )
        , ( "goodWill", E.int goodWill )
        , ( "paranoia", E.int paranoia )
        , ( "intrigue", E.int intrigue )
        , ( "location", ExEncode.maybe E.string <| Maybe.map Board.toString location )
        , ( "forbiddenLocations", E.list (E.string << Board.toString) forbiddenLocations )
        , ( "isDead", E.bool isDead )
        ]



-- ==============================================================================================
-- Method
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


setLocationBoard : Maybe Board -> Character -> Character
setLocationBoard b c =
    { c | location = b }


setGoodWill : Int -> Character -> Character
setGoodWill v c =
    { c | goodWill = v }


setParanoia : Int -> Character -> Character
setParanoia v c =
    { c | paranoia = v }


setIntrigue : Int -> Character -> Character
setIntrigue v c =
    { c | intrigue = v }


setIsDead : Bool -> Character -> Character
setIsDead b c =
    { c | isDead = b }


setForbiddenLocations : List Board -> Character -> Character
setForbiddenLocations b c =
    { c | forbiddenLocations = b }


getSelectedCharacterHands : List Hand -> Character -> List HandType
getSelectedCharacterHands hands c =
    if c.characterType /= Models.Character.Illusion then
        List.map .handType <| Hand.getSelectedCharacterHands c.characterType hands

    else
        case c.location of
            Nothing ->
                []

            Just b ->
                List.map .handType <| Hand.getSelectedBoardHands b.boardType hands


resolveCard : List Hand -> Character -> Character
resolveCard hands c =
    let
        -- _ =
        --     Debug.log "decodeUser" hands
        list =
            getSelectedCharacterHands hands c
    in
    c
        |> resolveMovementCard list
        |> resolveCardParameter hands


resolveCardParameter : List Hand -> Character -> Character
resolveCardParameter hands c =
    let
        list =
            getSelectedCharacterHands hands c
    in
    c
        |> resolveGoodwillCard list
        |> resolveParanoiaCard list
        |> guardParanoiaMinus
        |> resolveIntrigueCard list hands


resolveMovementCard : List HandType -> Character -> Character
resolveMovementCard list c =
    let
        validForbiddenBoard =
            validMove c
    in
    if (==) 0 <| List.length list then
        c

    else if List.member ForbidMovement list then
        c

    else if List.member MovementHorizontal list && List.member MovementVertical list then
        setLocationBoard (validForbiddenBoard <| moveDiagonal c.location) c

    else if List.member MovementDiagonal list && List.member MovementVertical list then
        setLocationBoard (validForbiddenBoard <| moveHorizontal c.location) c

    else if List.member MovementDiagonal list && List.member MovementHorizontal list then
        setLocationBoard (validForbiddenBoard <| moveVertical c.location) c

    else if List.member MovementVertical list then
        setLocationBoard (validForbiddenBoard <| moveVertical c.location) c

    else if List.member MovementHorizontal list then
        setLocationBoard (validForbiddenBoard <| moveHorizontal c.location) c

    else if List.member MovementDiagonal list then
        setLocationBoard (validForbiddenBoard <| moveDiagonal c.location) c

    else
        c


moveVertical : Maybe Board -> Maybe Board
moveVertical mb =
    case mb of
        Nothing ->
            Nothing

        Just b ->
            case b.boardType of
                City ->
                    Just Board.hospital

                Hospital ->
                    Just Board.city

                School ->
                    Just Board.shrine

                Shrine ->
                    Just Board.school


moveHorizontal : Maybe Board -> Maybe Board
moveHorizontal mb =
    case mb of
        Nothing ->
            Nothing

        Just b ->
            case b.boardType of
                City ->
                    Just Board.school

                Hospital ->
                    Just Board.shrine

                School ->
                    Just Board.city

                Shrine ->
                    Just Board.hospital


validMove : Character -> Maybe Board -> Maybe Board
validMove c mb =
    case mb of
        Nothing ->
            Nothing

        Just b ->
            if List.member b c.forbiddenLocations then
                c.location

            else
                mb


moveDiagonal : Maybe Board -> Maybe Board
moveDiagonal mb =
    case mb of
        Nothing ->
            Nothing

        Just b ->
            case b.boardType of
                City ->
                    Just Board.shrine

                Hospital ->
                    Just Board.school

                School ->
                    Just Board.hospital

                Shrine ->
                    Just Board.city


resolveGoodwillCard : List HandType -> Character -> Character
resolveGoodwillCard list c =
    if (==) 0 <| List.length list then
        c

    else if List.member ForbidGoodwill list then
        c

    else if List.member GoodwillPlus1 list then
        setGoodWill (c.goodWill + 1) c

    else if List.member GoodwillPlus2 list then
        setGoodWill (c.goodWill + 2) c

    else
        c


resolveParanoiaCard : List HandType -> Character -> Character
resolveParanoiaCard list c =
    if (==) 0 <| List.length list then
        c

    else if List.member ForbidParanoia list then
        c

    else if List.member ParanoiaPlus1 list then
        resolveParanoiaCard (ExList.remove ParanoiaPlus1 list) (setParanoia (c.paranoia + 1) c)

    else if List.member ParanoiaMinus1 list then
        resolveParanoiaCard (ExList.remove ParanoiaMinus1 list) (setParanoia (c.paranoia - 1) c)

    else
        c


guardParanoiaMinus : Character -> Character
guardParanoiaMinus c =
    if c.paranoia < 0 then
        setParanoia 0 c

    else
        c


resolveIntrigueCard : List HandType -> List Hand -> Character -> Character
resolveIntrigueCard list hands c =
    let
        isForbidIntrigue =
            ((==) 1 <| List.length <| List.filter (\t -> t == ForbidIntrigue) <| list)
                && ((==) 1 <| List.length <| List.filter (\t -> t == ForbidIntrigue) <| List.map .handType <| hands)
    in
    if (==) 0 <| List.length list then
        c

    else if List.member IntriguePlus1 list && not isForbidIntrigue then
        setIntrigue (c.intrigue + 1) c

    else if List.member IntriguePlus2 list && not isForbidIntrigue then
        setIntrigue (c.intrigue + 2) c

    else
        c



-- ==============================================================================================
-- getter
-- ==============================================================================================


getCharactersOnBoard : Board.BoardType -> List Character -> List Character
getCharactersOnBoard t list =
    list
        |> List.filter
            (\c ->
                case c.location of
                    Just l ->
                        if l.boardType == t then
                            True

                        else
                            False

                    Nothing ->
                        False
            )


getCharactersOnCity : List Character -> List Character
getCharactersOnCity list =
    getCharactersOnBoard Board.City list


getCharactersOnShrine : List Character -> List Character
getCharactersOnShrine list =
    getCharactersOnBoard Board.Shrine list


getCharactersOnHospital : List Character -> List Character
getCharactersOnHospital list =
    getCharactersOnBoard Board.Hospital list


getCharactersOnSchool : List Character -> List Character
getCharactersOnSchool list =
    getCharactersOnBoard Board.School list


isTurf : Board.BoardType -> List Character -> Bool
isTurf t list =
    list
        |> List.filter
            (\c ->
                case c.turf of
                    Just turf ->
                        turf.boardType == t

                    Nothing ->
                        False
            )
        |> List.length
        |> (/=) 0


isTurfHospital : List Character -> Bool
isTurfHospital list =
    isTurf Board.Hospital list


isTurfCity : List Character -> Bool
isTurfCity list =
    isTurf Board.City list


isTurfShrine : List Character -> Bool
isTurfShrine list =
    isTurf Board.Shrine list


isTurfSchool : List Character -> Bool
isTurfSchool list =
    isTurf Board.School list


filterTransferStudent : Int -> List Character -> List Character
filterTransferStudent i list =
    list
        |> List.filter
            (\c ->
                if c.characterType == Models.Character.TransferStudent then
                    case c.optionalNumber of
                        Just y ->
                            i >= y

                        Nothing ->
                            False

                else
                    True
            )


filterGodlyBeing : Int -> List Character -> List Character
filterGodlyBeing i list =
    list
        |> List.filter
            (\c ->
                if c.characterType == Models.Character.GodlyBeing then
                    case c.optionalNumber of
                        Just y ->
                            i >= y

                        Nothing ->
                            False

                else
                    True
            )


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
    List.map (\b -> Tuple.pair (Board.toString b) b.name) (UtilityList.exceptList c.forbiddenLocations Board.boards)


boardListWithNothing : Character -> List ( String, String )
boardListWithNothing c =
    ( "除外", "除外" ) :: boardList c


getFormOptionList : List CharacterType -> List Character -> List ( String, String )
getFormOptionList slectedTypes list =
    list
        |> List.filter (\c -> not <| List.member c.characterType slectedTypes)
        |> List.filter (\c -> c.characterType /= Models.Character.Illusion)
        |> List.map (\c -> ( Models.Character.characterTypeToString c.characterType, c.name ))
        |> List.reverse


getCharacter : CharacterType -> List Character -> Maybe Character
getCharacter t list =
    ExList.find (\c -> c.characterType == t) list



-- ==============================================================================================
-- View
-- ==============================================================================================


characterCard : Character -> Html msg
characterCard c =
    div [ class "rooper-roomdata-character-card" ]
        [ img [ src (characterToCardUrl c) ] []
        , if c.isDead then
            img [ src "/assets/images/others/x.png" ] []

          else
            text ""
        , characterCardChip c.goodWill "goodwill"
        , characterCardChip c.paranoia "paranoia"
        , characterCardChip c.intrigue "intrigue"
        ]


characterCardChip : Int -> String -> Html msg
characterCardChip i s =
    div [ class "rooper-roomdata-character-parameter" ] <|
        List.concat
            [ List.map
                (\_ -> span [ class <| "chip big " ++ s ] [ text "3" ])
                (List.range 1 (i // 3))
            , List.map
                (\_ -> span [ class <| "chip " ++ s ] [ text "" ])
                (List.range 1 (modBy 3 i))
            ]


charactersFormItem : Character -> (String -> msg) -> (String -> msg) -> (String -> msg) -> (String -> msg) -> msg -> msg -> Html msg
charactersFormItem c changeLocationMsg changeGMsg changePMsg changeIMsg toggleIsDeadMsg deleteForbiddenLocationMsg =
    div []
        [ div [ class "rooper-character-room-form-item" ]
            [ characterCard c
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
                , div [] [ input [ type_ "checkbox", checked c.isDead, onClick toggleIsDeadMsg ] [] ]
                ]
            ]
        , div []
            [ text c.name
            , if c.characterType == Models.Character.Patient && List.length c.forbiddenLocations /= 0 then
                button [ style "margin-left" "1rem", onClick deleteForbiddenLocationMsg ] [ text "退院" ]

              else if c.characterType == Models.Character.LittleGirl && List.length c.forbiddenLocations /= 0 then
                button [ style "margin-left" "1rem", onClick deleteForbiddenLocationMsg ] [ text "下校" ]

              else
                text ""
            ]
        ]


characterLocationBoards : Character -> (String -> msg) -> Html msg
characterLocationBoards char chgMsg =
    let
        boardKey =
            case char.location of
                Just b ->
                    Board.toString b

                Nothing ->
                    "除外"

        optionList =
            case char.characterType of
                Models.Character.Illusion ->
                    boardListWithNothing char

                _ ->
                    boardList char
    in
    Form.select ("bottom-form-character-board-" ++ toString char) chgMsg boardKey optionList
