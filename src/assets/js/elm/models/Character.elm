module Models.Character exposing (..)

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
import Models.TragedySet as TragedySet exposing (Role)


type CharacterType
    = BoyStudent -- 男子学生
    | GirlStudent -- 女子学生
    | RichMansDaughter -- お嬢様
    | ShrineMaiden -- 巫女
    | PoliceOfficer -- サラリーマン
    | OfficeWorker -- サラリーマン
    | Informer -- 情報屋
    | Doctor -- 医者
    | Patient -- 患者
    | ClassRep -- 委員長
    | MysteryBoy -- イレギュラー
    | Alien -- 異世界人
    | GodlyBeing -- 神格
    | PopIdol -- アイドル
    | Journalist -- マスコミ
    | Boss -- 大物
    | Nurse -- ナース
    | Henchman -- 手先
    | Illusion -- 幻想
    | Scientist -- 学者
    | ForensicSpecialist -- 鑑識官
    | AI -- A.I.
    | Teacher -- 教師
    | TransferStudent -- 転校生
    | Soldier -- 軍人
    | BlackCat -- 黒猫
    | LittleGirl -- 女の子
    | Sister -- 妹


type alias Character =
    { characterType : CharacterType
    , name : String
    , paranoiaLimit : Int
    , firstLocation : Board
    , forbiddenLocations : List Board -- 禁止エリア
    }


type alias CharacterScriptData =
    { character : Character
    , role : Maybe Role -- 役職
    , optionalNumber : Maybe Int -- 神格の場合、登場ループ数。転校生の場合、登場日数。
    , turf : Maybe Board -- 大物の場合のテリトリー
    }


type alias CharacterData =
    { scriptData : CharacterScriptData
    , goodWill : Int -- 友好
    , paranoia : Int -- 不安
    , intrigue : Int -- 暗躍
    , location : Maybe Board -- 現在のボード
    , forbiddenLocations : List Board
    }



-- Method CharacterScriptData


characterScriptDataFromCharacter : Character -> CharacterScriptData
characterScriptDataFromCharacter c =
    case c.characterType of
        GodlyBeing ->
            CharacterScriptData c Nothing (Just 1) Nothing

        TransferStudent ->
            CharacterScriptData c Nothing (Just 1) Nothing

        Boss ->
            CharacterScriptData c Nothing Nothing (Just Board.city)

        _ ->
            CharacterScriptData c Nothing Nothing Nothing



-- Method Character


characterToString : Character -> String
characterToString c =
    characterTypeToString c.characterType


characterTypeToString : CharacterType -> String
characterTypeToString t =
    case t of
        BoyStudent ->
            "BoyStudent"

        GirlStudent ->
            "GirlStudent"

        RichMansDaughter ->
            "RichMansDaughter"

        ShrineMaiden ->
            "ShrineMaiden"

        PoliceOfficer ->
            "PoliceOfficer"

        OfficeWorker ->
            "OfficeWorker"

        Informer ->
            "Informer"

        Doctor ->
            "Doctor"

        Patient ->
            "Patient"

        ClassRep ->
            "ClassRep"

        MysteryBoy ->
            "MysteryBoy"

        Alien ->
            "Alien"

        GodlyBeing ->
            "GodlyBeing"

        PopIdol ->
            "PopIdol"

        Journalist ->
            "Journalist"

        Boss ->
            "Boss"

        Nurse ->
            "Nurse"

        Henchman ->
            "Henchman"

        Illusion ->
            "Illusion"

        Scientist ->
            "Scientist"

        ForensicSpecialist ->
            "ForensicSpecialist"

        AI ->
            "AI"

        Teacher ->
            "Teacher"

        TransferStudent ->
            "TransferStudent"

        Soldier ->
            "Soldier"

        BlackCat ->
            "BlackCat"

        LittleGirl ->
            "LittleGirl"

        Sister ->
            "Sister"


characterFromString : String -> Maybe Character
characterFromString s =
    case s of
        "BoyStudent" ->
            Just boyStudent

        "GirlStudent" ->
            Just girlStudent

        "RichMansDaughter" ->
            Just richMansDaughter

        "ShrineMaiden" ->
            Just shrineMaiden

        "PoliceOfficer" ->
            Just policeOfficer

        "OfficeWorker" ->
            Just officeWorker

        "Informer" ->
            Just informer

        "Doctor" ->
            Just doctor

        "Patient" ->
            Just patient

        "ClassRep" ->
            Just classRep

        "MysteryBoy" ->
            Just mysteryBoy

        "Alien" ->
            Just alien

        "GodlyBeing" ->
            Just godlyBeing

        "PopIdol" ->
            Just popIdol

        "Journalist" ->
            Just journalist

        "Boss" ->
            Just boss

        "Nurse" ->
            Just nurse

        "Henchman" ->
            Just henchman

        "Illusion" ->
            Just illusion

        "Scientist" ->
            Just scientist

        "ForensicSpecialist" ->
            Just forensicSpecialist

        "AI" ->
            Just ai

        "Teacher" ->
            Just teacher

        "TransferStudent" ->
            Just transferStudent

        "Soldier" ->
            Just soldier

        "BlackCat" ->
            Just blackCat

        "LittleGirl" ->
            Just littleGirl

        "Sister" ->
            Just sister

        _ ->
            Nothing


characterTypeToName : CharacterType -> String
characterTypeToName t =
    characterFromCharacterType t |> .name


characterTypeFromString : String -> Maybe CharacterType
characterTypeFromString s =
    characterFromString s |> Maybe.map .characterType


characterFromCharacterType : CharacterType -> Character
characterFromCharacterType t =
    case t of
        BoyStudent ->
            boyStudent

        GirlStudent ->
            girlStudent

        RichMansDaughter ->
            richMansDaughter

        ShrineMaiden ->
            shrineMaiden

        PoliceOfficer ->
            policeOfficer

        OfficeWorker ->
            officeWorker

        Informer ->
            informer

        Doctor ->
            doctor

        Patient ->
            patient

        ClassRep ->
            classRep

        MysteryBoy ->
            mysteryBoy

        Alien ->
            alien

        GodlyBeing ->
            godlyBeing

        PopIdol ->
            popIdol

        Journalist ->
            journalist

        Boss ->
            boss

        Nurse ->
            nurse

        Henchman ->
            henchman

        Illusion ->
            illusion

        Scientist ->
            scientist

        ForensicSpecialist ->
            forensicSpecialist

        AI ->
            ai

        Teacher ->
            teacher

        TransferStudent ->
            transferStudent

        Soldier ->
            soldier

        BlackCat ->
            blackCat

        LittleGirl ->
            littleGirl

        Sister ->
            sister


characters : List Character
characters =
    [ boyStudent
    , girlStudent
    , richMansDaughter
    , shrineMaiden
    , policeOfficer
    , officeWorker
    , informer
    , doctor
    , patient
    , classRep
    , mysteryBoy
    , alien
    , godlyBeing
    , popIdol
    , journalist
    , boss
    , nurse
    , henchman
    , illusion
    , scientist
    , forensicSpecialist
    , ai
    , teacher
    , transferStudent
    , soldier
    , blackCat
    , littleGirl
    , sister
    ]


charactersFromCharacterScriptDataList : List CharacterScriptData -> List Character
charactersFromCharacterScriptDataList list =
    List.map (\data -> data.character) list


rolesFromCharacterScriptDataList : List CharacterScriptData -> List Role
rolesFromCharacterScriptDataList list =
    list
        |> List.filter (\data -> data.role /= Nothing)
        |> List.filter (\data -> data.character.characterType /= MysteryBoy)
        |> List.map (\data -> Maybe.withDefault TragedySet.person data.role)



-- テスト用


characterToScriptData : Character -> CharacterScriptData
characterToScriptData c =
    CharacterScriptData c Nothing Nothing Nothing



-- Method Decoder デコーダ
-- システムを通じて入れたfirebaseからの値のデコードを想定しているため失敗しない前提でとりあえず殺人計画をデフォルトにしておく


characterFromStringWithDefault : String -> Character
characterFromStringWithDefault =
    characterFromString >> Maybe.withDefault boyStudent


decoderCharacter : D.Decoder Character
decoderCharacter =
    D.map characterFromStringWithDefault D.string


decodeCharacterScriptData : D.Decoder CharacterScriptData
decodeCharacterScriptData =
    D.succeed CharacterScriptData
        |> Pipeline.required "character" decoderCharacter
        |> Pipeline.optional "role" TragedySet.decodeRole Nothing
        |> Pipeline.optional "optionalNumber" (D.maybe D.int) Nothing
        |> Pipeline.optional "turf" Board.decodeBoard Nothing



-- Method Encode エンコーダ


encodeCharacter : Character -> E.Value
encodeCharacter char =
    E.string <| characterToString char


encodeCharacterScriptData : CharacterScriptData -> E.Value
encodeCharacterScriptData data =
    E.object
        [ ( "character", encodeCharacter data.character )
        , ( "role", ExEncode.maybe E.string <| Maybe.map TragedySet.roleToString data.role )
        , ( "optionalNumber", ExEncode.maybe E.int data.optionalNumber )
        , ( "turf", ExEncode.maybe E.string <| Maybe.map Board.toString data.turf )
        ]



-- データ


boyStudent : Character
boyStudent =
    Character BoyStudent "男子学生" 2 Board.school []


girlStudent : Character
girlStudent =
    Character GirlStudent "女子学生" 3 Board.school []


richMansDaughter : Character
richMansDaughter =
    Character RichMansDaughter "お嬢様" 1 Board.school []


shrineMaiden : Character
shrineMaiden =
    Character ShrineMaiden "巫女" 2 Board.shrine [ Board.city ]


policeOfficer : Character
policeOfficer =
    Character PoliceOfficer "刑事" 2 Board.city []


officeWorker : Character
officeWorker =
    Character OfficeWorker "サラリーマン" 2 Board.city [ Board.school ]


informer : Character
informer =
    Character Informer "情報屋" 3 Board.city []


doctor : Character
doctor =
    Character Doctor "医者" 2 Board.hospital []


patient : Character
patient =
    Character Patient "患者" 2 Board.hospital [ Board.school, Board.shrine, Board.city ]


classRep : Character
classRep =
    Character ClassRep "委員長" 2 Board.school []


mysteryBoy : Character
mysteryBoy =
    Character MysteryBoy "イレギュラー" 3 Board.school []


alien : Character
alien =
    Character Alien "異世界人" 2 Board.shrine [ Board.hospital ]


godlyBeing : Character
godlyBeing =
    Character GodlyBeing "神格" 3 Board.shrine []


popIdol : Character
popIdol =
    Character PopIdol "アイドル" 2 Board.city []


journalist : Character
journalist =
    Character Journalist "マスコミ" 2 Board.city []


boss : Character
boss =
    Character Boss "大物" 4 Board.city []


nurse : Character
nurse =
    Character Nurse "ナース" 2 Board.hospital []


henchman : Character
henchman =
    Character Henchman "手先" 1 Board.shrine []


illusion : Character
illusion =
    Character Illusion "幻想" 3 Board.shrine []


scientist : Character
scientist =
    Character Scientist "学者" 2 Board.hospital []


forensicSpecialist : Character
forensicSpecialist =
    Character ForensicSpecialist "鑑識官" 3 Board.city []


ai : Character
ai =
    Character AI "A.I." 4 Board.city []


teacher : Character
teacher =
    Character Teacher "教師" 2 Board.school []


transferStudent : Character
transferStudent =
    Character TransferStudent "転校生" 2 Board.school []


soldier : Character
soldier =
    Character Soldier "軍人" 3 Board.hospital []


blackCat : Character
blackCat =
    Character BlackCat "黒猫" 0 Board.shrine []


littleGirl : Character
littleGirl =
    Character LittleGirl "女の子" 1 Board.school [ Board.shrine, Board.hospital, Board.city ]


sister : Character
sister =
    Character Sister "妹" 3 Board.shrine []



-- View Method


characterToCardUrl : Character -> String
characterToCardUrl c =
    characterTypeToCardUrl c.characterType


characterTypeToCardUrl : CharacterType -> String
characterTypeToCardUrl t =
    "/assets/images/characters/"
        ++ characterTypeToCardFileName t


characterTypeToLargeCardUrl : CharacterType -> String
characterTypeToLargeCardUrl t =
    "/assets/images/characters/large/"
        ++ characterTypeToCardFileName t


characterTypeToCardFileName : CharacterType -> String
characterTypeToCardFileName t =
    case t of
        BoyStudent ->
            "boy-student.png"

        GirlStudent ->
            "girl-student.png"

        RichMansDaughter ->
            "rich-mans-daughter.png"

        ShrineMaiden ->
            "shrine-maiden.png"

        PoliceOfficer ->
            "pollice-officer.png"

        OfficeWorker ->
            "office-worker.png"

        Informer ->
            "informer.png"

        Doctor ->
            "doctor.png"

        Patient ->
            "patient.png"

        ClassRep ->
            "class-rep.png"

        MysteryBoy ->
            "mystery-boy.png"

        Alien ->
            "alien.png"

        GodlyBeing ->
            "godly-being.png"

        PopIdol ->
            "pop-idol.png"

        Journalist ->
            "journalist.png"

        Boss ->
            "boss.png"

        Nurse ->
            "nurse.png"

        Henchman ->
            "henchman.png"

        Illusion ->
            "illusion.png"

        Scientist ->
            "scientist.png"

        ForensicSpecialist ->
            "forensic-specialist.png"

        AI ->
            "ai.png"

        Teacher ->
            "teacher.png"

        TransferStudent ->
            "transfer-student.png"

        Soldier ->
            "soldier.png"

        BlackCat ->
            "black-cat.png"

        LittleGirl ->
            "little-girl.png"

        Sister ->
            "sister.png"



-- View


characterNameCard : msg -> Character -> Bool -> Html msg
characterNameCard clickMsg c isSelected =
    let
        borderAttr =
            if isSelected then
                [ class "rooper-character-card-is-active" ]

            else
                []
    in
    div (List.append [ class "rooper-character-card", onClick clickMsg ] borderAttr)
        [ figure [ class "image" ]
            [ img [ src (characterToCardUrl c) ] []
            ]
        , div [] [ text c.name ]
        ]


characterFormCollectionItem : (CharacterType -> msg) -> CharacterScriptData -> List (Html msg) -> Html msg
characterFormCollectionItem modalMsg { character } children =
    div [ class "media" ]
        [ div [ class "media-left", onClick (modalMsg character.characterType), class "rooper-character-item-wrapper" ]
            [ img [ src (characterToCardUrl character) ] []
            , div [ class "rooper-expand-icon-wrapper" ] [ i [ class "fas fa-expand-arrows-alt" ] [] ]
            , div [ style "min-width" "7rem" ] [ text character.name ]
            ]
        , div [ class "media-content" ] children
        ]


characterTurfBoards : CharacterScriptData -> (String -> msg) -> Html msg
characterTurfBoards char chgMsg =
    let
        boardKey =
            Board.toString (Maybe.withDefault Board.city char.turf)

        optionList =
            List.map (\b -> Tuple.pair (Board.toString b) b.name) Board.boards
    in
    Form.select ("-character-board-" ++ characterToString char.character) chgMsg boardKey optionList
