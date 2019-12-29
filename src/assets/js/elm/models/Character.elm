module Models.Character exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models.Board as Board exposing (BoardType(..))
import Models.TragedySet exposing (Role)


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


type alias Character =
    { characterType : CharacterType
    , name : String
    , paranoiaLimit : Int
    , firstLocation : BoardType
    }


type alias CharacterScriptData =
    { character : Character
    , role : Maybe Role -- 役職
    , optionalNumber : Maybe Int -- 神格の場合、登場ループ数。転校生の場合、登場日数。
    , turf : Maybe BoardType -- 大物の場合のテリトリー
    }


type alias CharacterData =
    { character : CharacterScriptData
    , goodWill : Int -- 友好
    , paranoia : Int -- 不安
    , intrigue : Int -- 暗躍
    , location : Maybe BoardType -- 現在のボード
    , forbiddenLocations : List BoardType
    }



-- Method


characterToString : Character -> String
characterToString c =
    case c.characterType of
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

        _ ->
            Nothing


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
    ]



-- データ


boyStudent : Character
boyStudent =
    Character BoyStudent "男子学生" 2 School


girlStudent : Character
girlStudent =
    Character GirlStudent "女子学生" 3 School


richMansDaughter : Character
richMansDaughter =
    Character RichMansDaughter "お嬢様" 1 School


shrineMaiden : Character
shrineMaiden =
    Character ShrineMaiden "巫女" 2 Shrine


policeOfficer : Character
policeOfficer =
    Character PoliceOfficer "刑事" 2 City


officeWorker : Character
officeWorker =
    Character OfficeWorker "サラリーマン" 2 City


informer : Character
informer =
    Character Informer "情報屋" 3 City


doctor : Character
doctor =
    Character Doctor "医者" 2 Hospital


patient : Character
patient =
    Character Patient "患者" 2 Hospital


classRep : Character
classRep =
    Character ClassRep "委員長" 2 School


mysteryBoy : Character
mysteryBoy =
    Character MysteryBoy "イレギュラー" 3 School


alien : Character
alien =
    Character Alien "異世界人" 2 Shrine


godlyBeing : Character
godlyBeing =
    Character GodlyBeing "神格" 3 Shrine


popIdol : Character
popIdol =
    Character PopIdol "アイドル" 2 City


journalist : Character
journalist =
    Character Journalist "マスコミ" 2 City


boss : Character
boss =
    Character Boss "大物" 4 City


nurse : Character
nurse =
    Character Nurse "ナース" 2 Hospital


henchman : Character
henchman =
    Character Henchman "手先" 1 Shrine


illusion : Character
illusion =
    Character Illusion "幻想" 3 Shrine


scientist : Character
scientist =
    Character Scientist "学者" 2 Hospital


forensicSpecialist : Character
forensicSpecialist =
    Character ForensicSpecialist "鑑識官" 3 City


ai : Character
ai =
    Character AI "A.I." 4 City


teacher : Character
teacher =
    Character Teacher "教師" 2 School


transferStudent : Character
transferStudent =
    Character TransferStudent "転校生" 2 School


soldier : Character
soldier =
    Character Soldier "軍人" 3 Hospital


blackCat : Character
blackCat =
    Character BlackCat "黒猫" 0 Shrine


littleGirl : Character
littleGirl =
    Character LittleGirl "女の子" 1 School



-- View Method


characterToCardUrl : Character -> String
characterToCardUrl c =
    let
        filename =
            case c.characterType of
                BoyStudent ->
                    "char0.png"

                GirlStudent ->
                    "char1.png"

                RichMansDaughter ->
                    "char2.png"

                ShrineMaiden ->
                    "char3.png"

                PoliceOfficer ->
                    "char4.png"

                OfficeWorker ->
                    "char5.png"

                Informer ->
                    "char6.png"

                Doctor ->
                    "char7.png"

                Patient ->
                    "char8.png"

                ClassRep ->
                    "char9.png"

                MysteryBoy ->
                    "char10.png"

                Alien ->
                    "char11.png"

                GodlyBeing ->
                    "char12.png"

                PopIdol ->
                    "char13.png"

                Journalist ->
                    "char14.png"

                Boss ->
                    "char15.png"

                Nurse ->
                    "char16.png"

                Henchman ->
                    "char17.png"

                Illusion ->
                    "char19.png"

                Scientist ->
                    "char18.png"

                ForensicSpecialist ->
                    "char20.png"

                AI ->
                    "char21.png"

                Teacher ->
                    "silhouette.png"

                TransferStudent ->
                    "silhouette.png"

                Soldier ->
                    "silhouette.png"

                BlackCat ->
                    "silhouette.png"

                LittleGirl ->
                    "silhouette.png"
    in
    "/assets/images/characters/" ++ filename



-- View


characterNameCard : Character -> Bool -> Html msg
characterNameCard c isSelected =
    let
        borderAttr =
            if isSelected then
                [ style "border" "solid yellow 2px" ]

            else
                [ style "border" "solid #fff 1px", style "opacity" "0.5" ]
    in
    div (List.append [ style "width" "90px", style "margin-left" "5px", style "margin-bottom" "10px" ] borderAttr)
        [ figure [ class "image", style "width" "35px", style "height" "50px", style "margin" "0 auto" ]
            [ img [ src (characterToCardUrl c) ] []
            ]
        , div [ style "text-align" "center" ] [ text c.name ]
        ]
