module Models.TragedySet exposing (..)

import Form.Decoder as Decoder exposing (Decoder, Validator)
import Html exposing (..)
import Json.Decode as D exposing (Value)
import Json.Decode.Pipeline
import Json.Encode as E
import List.Extra as ExList


type RoleType
    = Person
    | Killer
    | Brain
    | KeyPerson
    | Cultist
    | TimeTraveler
    | Witch
    | Friend
    | LovedOne
    | Lover
    | SerialKiller
    | Factor
    | ConspiracyTheorist
    | Curmudgeon
    | Poisoner
    | Fool
    | Paranoiac
    | Therapist
    | PrivateInvestigator
    | Twin
    | Obstinate


type alias Role =
    { roleType : RoleType
    , name : String
    , limit : Maybe Int
    }



-- Role Method
-- Role Decoder


decodeRole : D.Decoder (Maybe Role)
decodeRole =
    D.map roleFromString D.string


roleToString : Role -> String
roleToString r =
    case r.roleType of
        Person ->
            "Person"

        Killer ->
            "Killer"

        Brain ->
            "Brain"

        KeyPerson ->
            "KeyPerson"

        Cultist ->
            "Cultist"

        TimeTraveler ->
            "TimeTraveler"

        Witch ->
            "Witch"

        Friend ->
            "Friend"

        LovedOne ->
            "LovedOne"

        Lover ->
            "Lover"

        SerialKiller ->
            "SerialKiller"

        Factor ->
            "Factor"

        ConspiracyTheorist ->
            "ConspiracyTheorist"

        Curmudgeon ->
            "Curmudgeon"

        Poisoner ->
            "Poisoner"

        Fool ->
            "Fool"

        Paranoiac ->
            "Paranoiac"

        Therapist ->
            "Therapist"

        PrivateInvestigator ->
            "PrivateInvestigator"

        Twin ->
            "Twin"

        Obstinate ->
            "Obstinate"


roleFromString : String -> Maybe Role
roleFromString s =
    case s of
        "Person" ->
            Nothing

        "Killer" ->
            Just killer

        "Brain" ->
            Just brain

        "KeyPerson" ->
            Just keyPerson

        "Cultist" ->
            Just cultist

        "TimeTraveler" ->
            Just timeTraveler

        "Witch" ->
            Just witch

        "Friend" ->
            Just friend

        "LovedOne" ->
            Just lovedOne

        "Lover" ->
            Just lover

        "SerialKiller" ->
            Just serialKiller

        "Factor" ->
            Just factor

        "ConspiracyTheorist" ->
            Just conspiracyTheorist

        "Curmudgeon" ->
            Just curmudgeon

        "Poisoner" ->
            Just poisoner

        "Fool" ->
            Just fool

        "Paranoiac" ->
            Just paranoiac

        "Therapist" ->
            Just therapist

        "PrivateInvestigator" ->
            Just privateInvestigator

        "Twin" ->
            Just twin

        "Obstinate" ->
            Just obstinate

        _ ->
            Nothing


filterRoleLimit : List Role -> List Role
filterRoleLimit list =
    filterRoleLimitHelp list []


filterRoleLimitHelp : List Role -> List Role -> List Role
filterRoleLimitHelp remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            case first.limit of
                Nothing ->
                    filterRoleLimitHelp rest (first :: accumulator)

                Just limit ->
                    let
                        cnt =
                            ExList.count ((==) first) accumulator
                    in
                    if cnt >= limit then
                        filterRoleLimitHelp rest accumulator

                    else
                        filterRoleLimitHelp rest (first :: accumulator)



-- ルール


type alias Plot =
    { name : String
    , isMain : Bool
    , plotType : PlotType
    , roles : List Role
    , effects : List Effect
    }


type PlotType
    = MurderPlan
    | LightOfTheAvenger
    | APlaceToProtect
    | TheSealedItem
    | SignWithMe
    | ChangeOfFuture
    | GiantTimeBomb
    | AnUnsettlingRumour
    | AHideousScript
    | ShadowOfTheRipper
    | CircleOfFriends
    | ALoveAffair
    | TheHiddenFreak
    | ParanoiaVirus
    | ThreadsOfFate
    | UnknownFactorX
    | AQuiltOfIncidents
    | TightropePlan
    | TheBlackSchool
    | ADropOfStrychnine
    | IsolatedInstitutionPsycho
    | SmellOfGunpowder
    | IAmAMasterDetective
    | DanceOfFools
    | AnAbsoluteWill
    | TrickyTwins


type alias Effect =
    { effectType : EffectType
    , timing : Timing
    , oncePerLoop : Bool -- ループ1回
    , effect : String
    }


type EffectType
    = Mandatory -- 強制
    | Optional -- 任意
    | LossCondition -- 敗北条件


type CastType
    = Unkillable -- 不死
    | GoodwillRefusal -- 友好無視
    | MandatoryGoodwillRefusal -- 絶対友好無視


type Timing
    = LoopEnd -- ループ終了時
    | LoopStart -- ループ開始時
    | MastermindAbility -- 脚本家能力フェイズ
    | Always -- 常時
    | DayEnd -- ターン終了フェイズ
    | WritingScript -- 脚本作成時



-- ルール > メソッド


filterMainPlots : List Plot -> List Plot
filterMainPlots plots =
    List.filter (\plot -> plot.isMain) plots


filterSubPlots : List Plot -> List Plot
filterSubPlots plots =
    List.filter (\plot -> not plot.isMain) plots


plotToString : Plot -> String
plotToString p =
    case p.plotType of
        MurderPlan ->
            "MurderPlan"

        LightOfTheAvenger ->
            "LightOfTheAvenger"

        APlaceToProtect ->
            "APlaceToProtect"

        TheSealedItem ->
            "TheSealedItem"

        SignWithMe ->
            "SignWithMe"

        ChangeOfFuture ->
            "ChangeOfFuture"

        GiantTimeBomb ->
            "GiantTimeBomb"

        AnUnsettlingRumour ->
            "AnUnsettlingRumour"

        AHideousScript ->
            "AHideousScript"

        ShadowOfTheRipper ->
            "ShadowOfTheRipper"

        CircleOfFriends ->
            "CircleOfFriends"

        ALoveAffair ->
            "ALoveAffair"

        TheHiddenFreak ->
            "TheHiddenFreak"

        ParanoiaVirus ->
            "ParanoiaVirus"

        ThreadsOfFate ->
            "ThreadsOfFate"

        UnknownFactorX ->
            "UnknownFactorX"

        AQuiltOfIncidents ->
            "AQuiltOfIncidents"

        TightropePlan ->
            "TightropePlan"

        TheBlackSchool ->
            "TheBlackSchool"

        ADropOfStrychnine ->
            "ADropOfStrychnine"

        IsolatedInstitutionPsycho ->
            "IsolatedInstitutionPsycho"

        SmellOfGunpowder ->
            "SmellOfGunpowder"

        IAmAMasterDetective ->
            "IAmAMasterDetective"

        DanceOfFools ->
            "DanceOfFools"

        AnAbsoluteWill ->
            "AnAbsoluteWill"

        TrickyTwins ->
            "TrickyTwins"


plotFromString : String -> Maybe Plot
plotFromString s =
    case s of
        "MurderPlan" ->
            Just murderPlan

        "LightOfTheAvenger" ->
            Just lightOfTheAvenger

        "APlaceToProtect" ->
            Just aPlaceToProtect

        "TheSealedItem" ->
            Just theSealedItem

        "SignWithMe" ->
            Just signWithMe

        "ChangeOfFuture" ->
            Just changeOfFuture

        "GiantTimeBomb" ->
            Just giantTimeBomb

        "AnUnsettlingRumour" ->
            Just anUnsettlingRumour

        "AHideousScript" ->
            Just aHideousScript

        "ShadowOfTheRipper" ->
            Just shadowOfTheRipper

        "CircleOfFriends" ->
            Just circleOfFriends

        "ALoveAffair" ->
            Just aLoveAffair

        "TheHiddenFreak" ->
            Just theHiddenFreak

        "ParanoiaVirus" ->
            Just paranoiaVirus

        "ThreadsOfFate" ->
            Just threadsOfFate

        "UnknownFactorX" ->
            Just unknownFactorX

        "AQuiltOfIncidents" ->
            Just aQuiltOfIncidents

        "TightropePlan" ->
            Just tightropePlan

        "TheBlackSchool" ->
            Just theBlackSchool

        "ADropOfStrychnine" ->
            Just aDropOfStrychnine

        "IsolatedInstitutionPsycho" ->
            Just isolatedInstitutionPsycho

        "SmellOfGunpowder" ->
            Just smellOfGunpowder

        "IAmAMasterDetective" ->
            Just iAmAMasterDetective

        "DanceOfFools" ->
            Just danceOfFools

        "AnAbsoluteWill" ->
            Just anAbsoluteWill

        "TrickyTwins" ->
            Just trickyTwins

        _ ->
            Nothing



-- システムを通じて入れたfirebaseからの値のデコードを想定しているため失敗しない前提でとりあえず殺人計画をデフォルトにしておく


plotFromStringWithDefault : String -> Plot
plotFromStringWithDefault =
    plotFromString >> Maybe.withDefault murderPlan


decoderPlot : D.Decoder Plot
decoderPlot =
    D.map plotFromStringWithDefault D.string


decoderMaybePlot : D.Decoder (Maybe Plot)
decoderMaybePlot =
    D.map plotFromString D.string



-- 事件


type alias Incident =
    { incidentType : IncidentType
    , name : String
    , effect : String
    }


type IncidentType
    = Murder
    | IncreasingUnease
    | Suicide
    | HospitalIncident
    | FarawayMurder
    | MissingPerson
    | Spreading
    | FoulEvil
    | ButterflyEffect
    | SerialMurder
    | FakedSuicide
    | Terrorism
    | Portent
    | BestialMurder
    | ASuspiciousLetter
    | ClosedCircle
    | TheSilverBullet



-- 事件 ＞ メソッド


incidentToString : Incident -> String
incidentToString i =
    case i.incidentType of
        Murder ->
            "Murder"

        IncreasingUnease ->
            "IncreasingUnease"

        Suicide ->
            "Suicide"

        HospitalIncident ->
            "HospitalIncident"

        FarawayMurder ->
            "FarawayMurder"

        MissingPerson ->
            "MissingPerson"

        Spreading ->
            "Spreading"

        FoulEvil ->
            "FoulEvil"

        ButterflyEffect ->
            "ButterflyEffect"

        SerialMurder ->
            "SerialMurder"

        FakedSuicide ->
            "FakedSuicide"

        Terrorism ->
            "Terrorism"

        Portent ->
            "Portent"

        BestialMurder ->
            "BestialMurder"

        ASuspiciousLetter ->
            "ASuspiciousLetter"

        ClosedCircle ->
            "ClosedCircle"

        TheSilverBullet ->
            "TheSilverBullet"


incidentFromString : String -> Maybe Incident
incidentFromString s =
    case s of
        "Murder" ->
            Just murder

        "IncreasingUnease" ->
            Just increasingUnease

        "Suicide" ->
            Just suicide

        "HospitalIncident" ->
            Just hospitalIncident

        "FarawayMurder" ->
            Just farawayMurder

        "MissingPerson" ->
            Just missingPerson

        "Spreading" ->
            Just spreading

        "FoulEvil" ->
            Just foulEvil

        "ButterflyEffect" ->
            Just butterflyEffect

        "SerialMurder" ->
            Just serialMurder

        "FakedSuicide" ->
            Just fakedSuicide

        "Terrorism" ->
            Just terrorism

        "Portent" ->
            Just portent

        "BestialMurder" ->
            Just bestialMurder

        "ASuspiciousLetter" ->
            Just aSuspiciousLetter

        "ClosedCircle" ->
            Just closedCircle

        "TheSilverBullet" ->
            Just theSilverBullet

        _ ->
            Nothing



-- ルール ＞ データ


murderPlan : Plot
murderPlan =
    Plot "殺人計画" True MurderPlan [ killer, brain, keyPerson ] []


lightOfTheAvenger : Plot
lightOfTheAvenger =
    Plot "復讐者の灯火" True LightOfTheAvenger [ brain ] [ Effect LossCondition LoopEnd False "クロマクの初期エリアに[暗躍カウンター]が２つ以上ある" ]


aPlaceToProtect : Plot
aPlaceToProtect =
    Plot "守るべき場所" True APlaceToProtect [ keyPerson, cultist ] [ Effect LossCondition LoopEnd False "学校に[暗躍カウンター]が２つ以上ある。" ]


theSealedItem : Plot
theSealedItem =
    Plot "封印されしモノ" True TheSealedItem [ brain, cultist ] [ Effect LossCondition LoopEnd False "神社に[暗躍カウンター]が２つ以上ある。" ]


signWithMe : Plot
signWithMe =
    Plot "僕と契約しようよ！"
        True
        SignWithMe
        [ keyPerson ]
        [ Effect LossCondition LoopEnd False "キーパーソンに[暗躍カウンター]が２個以上。"
        , Effect Mandatory WritingScript False "キーパーソンは少女にしなくてはならない"
        ]


changeOfFuture : Plot
changeOfFuture =
    Plot "未来改変プラン" True ChangeOfFuture [ timeTraveler, cultist ] [ Effect LossCondition LoopEnd False "このループ中に「事件：蝶の羽ばたき」が発生している。" ]


giantTimeBomb : Plot
giantTimeBomb =
    Plot "巨大時限爆弾Xの存在" True GiantTimeBomb [ witch ] [ Effect LossCondition LoopEnd False "ウィッチの初期エリアに[暗躍カウンター]が２つ以上。" ]


anUnsettlingRumour : Plot
anUnsettlingRumour =
    Plot "不穏な噂" False AnUnsettlingRumour [ conspiracyTheorist ] [ Effect Optional MastermindAbility True "【脚本家能力フェイズ】任意のボード１つに[暗躍カウンター]を１つ置く。" ]


aHideousScript : Plot
aHideousScript =
    Plot "最低の却本" False AHideousScript [ friend, curmudgeon, curmudgeon, conspiracyTheorist ] [ Effect Optional WritingScript False "マイナスを２人ではなく、１人や０人追加してもよい。" ]


shadowOfTheRipper : Plot
shadowOfTheRipper =
    Plot "切り裂き魔の影" False ShadowOfTheRipper [ serialKiller, conspiracyTheorist ] []


circleOfFriends : Plot
circleOfFriends =
    Plot "友情サークル" False CircleOfFriends [ friend, friend, conspiracyTheorist ] []


aLoveAffair : Plot
aLoveAffair =
    Plot "恋愛風景" False ALoveAffair [ lovedOne, lover ] []


theHiddenFreak : Plot
theHiddenFreak =
    Plot "潜む殺人鬼" False TheHiddenFreak [ friend, serialKiller ] []


paranoiaVirus : Plot
paranoiaVirus =
    Plot "妄想拡大ウイルス" False ParanoiaVirus [ conspiracyTheorist ] [ Effect Mandatory Always False "パーソンは[不安カウンター]が３つ以上置かれている限り、役職がシリアルキラーに変更される。" ]


threadsOfFate : Plot
threadsOfFate =
    Plot "因果の糸" False ThreadsOfFate [] [ Effect Mandatory LoopStart False "ひとつ前のループ終了時に[友好カウンター]が置かれていた全キャラクターに[不安カウンター]を２つ置く。" ]


unknownFactorX : Plot
unknownFactorX =
    Plot "不定因子χ" False UnknownFactorX [ factor ] []



-- Mystery Circle


aQuiltOfIncidents : Plot
aQuiltOfIncidents =
    Plot "組み重なり事件キルト" True AQuiltOfIncidents [ fool, conspiracyTheorist ] []


tightropePlan : Plot
tightropePlan =
    Plot "タイトロープ上の計画\t" True TightropePlan [ killer, brain ] []


theBlackSchool : Plot
theBlackSchool =
    Plot "黒の学園" True TheBlackSchool [ brain ] []


aDropOfStrychnine : Plot
aDropOfStrychnine =
    Plot "ストリキニーネの雫" True ADropOfStrychnine [ keyPerson, poisoner, fool ] []


isolatedInstitutionPsycho : Plot
isolatedInstitutionPsycho =
    Plot "隔離病棟サイコ" False IsolatedInstitutionPsycho [ conspiracyTheorist, paranoiac, therapist ] []


smellOfGunpowder : Plot
smellOfGunpowder =
    Plot "火薬の香り" False SmellOfGunpowder [ serialKiller ] []


iAmAMasterDetective : Plot
iAmAMasterDetective =
    Plot "私は名探偵" False IAmAMasterDetective [ conspiracyTheorist, friend, privateInvestigator ] []


danceOfFools : Plot
danceOfFools =
    Plot "愚者のダンス" False DanceOfFools [ fool, friend ] []


anAbsoluteWill : Plot
anAbsoluteWill =
    Plot "絶対の意志" False AnAbsoluteWill [ obstinate ] []


trickyTwins : Plot
trickyTwins =
    Plot "双子のトリック" False TrickyTwins [ paranoiac, twin ] []


initBasicPlots : List Plot
initBasicPlots =
    [ murderPlan
    , theSealedItem
    , signWithMe
    , changeOfFuture
    , giantTimeBomb
    , circleOfFriends
    , aLoveAffair
    , theHiddenFreak
    , anUnsettlingRumour
    , paranoiaVirus
    , threadsOfFate
    , unknownFactorX
    ]


initFirstStepsPlots : List Plot
initFirstStepsPlots =
    [ murderPlan
    , lightOfTheAvenger
    , aPlaceToProtect
    , shadowOfTheRipper
    , anUnsettlingRumour
    , aHideousScript
    ]


initMysteryCirclePlots : List Plot
initMysteryCirclePlots =
    [ murderPlan
    , aQuiltOfIncidents
    , tightropePlan
    , theBlackSchool
    , aDropOfStrychnine
    , isolatedInstitutionPsycho
    , smellOfGunpowder
    , iAmAMasterDetective
    , danceOfFools
    , anAbsoluteWill
    , trickyTwins
    ]



-- 役職 データ


person : Role
person =
    Role Person "パーソン" Nothing


killer : Role
killer =
    Role Killer "キラー" Nothing


brain : Role
brain =
    Role Brain "クロマク" Nothing


keyPerson : Role
keyPerson =
    Role KeyPerson "キーパーソン" Nothing


cultist : Role
cultist =
    Role Cultist "カルティスト" Nothing


timeTraveler : Role
timeTraveler =
    Role TimeTraveler "タイムトラベラー" Nothing


witch : Role
witch =
    Role Witch "ウィッチ" Nothing


friend : Role
friend =
    Role Friend "フレンド" (Just 2)


lovedOne : Role
lovedOne =
    Role LovedOne "メインラバーズ" Nothing


lover : Role
lover =
    Role Lover "ラバーズ" Nothing


serialKiller : Role
serialKiller =
    Role SerialKiller "シリアルキラー" Nothing


factor : Role
factor =
    Role Factor "ファクター" Nothing


conspiracyTheorist : Role
conspiracyTheorist =
    Role ConspiracyTheorist "ミスリーダー" (Just 1)


curmudgeon : Role
curmudgeon =
    Role Curmudgeon "マイナス" Nothing


poisoner : Role
poisoner =
    Role Poisoner "ドリッパー" Nothing


fool : Role
fool =
    Role Fool "フール" (Just 1)


paranoiac : Role
paranoiac =
    Role Paranoiac "パラノイア" Nothing


therapist : Role
therapist =
    Role Therapist "セラピスト" Nothing


privateInvestigator : Role
privateInvestigator =
    Role PrivateInvestigator "メイタンテイ" Nothing


twin : Role
twin =
    Role Twin "ツイン" Nothing


obstinate : Role
obstinate =
    Role Obstinate "ゼッタイシャ" Nothing


initFirstStepsRoles : List Role
initFirstStepsRoles =
    [ killer
    , brain
    , keyPerson
    , cultist
    , friend
    , serialKiller
    , conspiracyTheorist
    ]


initBasicTragedyRoles : List Role
initBasicTragedyRoles =
    [ killer
    , brain
    , keyPerson
    , cultist
    , timeTraveler
    , witch
    , friend
    , lovedOne
    , lover
    , serialKiller
    , factor
    , conspiracyTheorist
    ]


initMysteryCircleRoles : List Role
initMysteryCircleRoles =
    [ keyPerson
    , killer
    , brain
    , poisoner
    , fool
    , conspiracyTheorist
    , friend
    , serialKiller
    , paranoiac
    , therapist
    , privateInvestigator
    , obstinate
    , twin
    ]



-- 事件 データ


murder : Incident
murder =
    Incident Murder "殺人事件" "犯人と同一エリアにいる犯人以外の任意のキャラクター１人を死亡させる。"


increasingUnease : Incident
increasingUnease =
    Incident IncreasingUnease "不安拡大" "任意のキャラクター１人に不安カウンターを２つ置き、任意の別のキャラクター１人に暗躍カウンターを１つ置く。"


suicide : Incident
suicide =
    Incident Suicide "自殺" "犯人は死亡する。"


hospitalIncident : Incident
hospitalIncident =
    Incident HospitalIncident "病院の事件" "病院に[暗躍カウンター]が１つ以上→病院にいる全てのキャラクターを死亡させる。病院に[暗躍カウンター]が２つ以上→主人公を死亡させる。"


farawayMurder : Incident
farawayMurder =
    Incident FarawayMurder "遠隔殺人" "[暗躍カウンター]が２つ以上置かれたキャラクター１人を死亡させる"


missingPerson : Incident
missingPerson =
    Incident MissingPerson "行方不明" "犯人を任意のボードに移動させる。その後、犯人のいるボードに[暗躍カウンター]を１つ置く"


spreading : Incident
spreading =
    Incident Spreading "流布" "任意のキャラクター１人から[友好カウンター]を２つ取り除き、別のキャラクター１人に[友好カウンター]を２つ置く。"


foulEvil : Incident
foulEvil =
    Incident FoulEvil "邪気の汚染" "神社に[暗躍カウンター]を２つ置く。"


butterflyEffect : Incident
butterflyEffect =
    Incident ButterflyEffect "蝶の羽ばたき" "犯人と同一エリアにいるキャラクター1人にいずれかのカウンターを１つ置く。"



-- Mystery Circle


serialMurder : Incident
serialMurder =
    Incident SerialMurder "連続殺人" ""


fakedSuicide : Incident
fakedSuicide =
    Incident FakedSuicide "偽装自殺" ""


terrorism : Incident
terrorism =
    Incident Terrorism "テロリズム" ""


portent : Incident
portent =
    Incident Portent "前兆" ""


bestialMurder : Incident
bestialMurder =
    Incident BestialMurder "猟奇殺人" ""


aSuspiciousLetter : Incident
aSuspiciousLetter =
    Incident ASuspiciousLetter "不審な手紙" ""


closedCircle : Incident
closedCircle =
    Incident ClosedCircle "クローズドサークル" ""


theSilverBullet : Incident
theSilverBullet =
    Incident TheSilverBullet "銀の銃弾" ""


initBasicTragedyIncidents : List Incident
initBasicTragedyIncidents =
    [ murder
    , increasingUnease
    , suicide
    , hospitalIncident
    , farawayMurder
    , missingPerson
    , spreading
    , foulEvil
    , butterflyEffect
    ]


initFirstStepsIncidents : List Incident
initFirstStepsIncidents =
    [ murder
    , increasingUnease
    , suicide
    , hospitalIncident
    , farawayMurder
    , missingPerson
    , spreading
    ]


initMysteryCircleIncidents : List Incident
initMysteryCircleIncidents =
    [ serialMurder
    , terrorism
    , hospitalIncident
    , suicide
    , increasingUnease
    , portent
    , bestialMurder
    , fakedSuicide
    , closedCircle
    , theSilverBullet
    ]


type alias TragedySet =
    { name : String
    , subPlotNumber : Int
    , plots : List Plot
    , roles : List Role
    , incidents : List Incident
    , setType : TragedySetType
    }


initFirstSteps : TragedySet
initFirstSteps =
    TragedySet "First Steps" 1 initFirstStepsPlots initFirstStepsRoles initFirstStepsIncidents FirstSteps


initBasicTragedy : TragedySet
initBasicTragedy =
    TragedySet "Basic Tragedy X" 2 initBasicPlots initBasicTragedyRoles initBasicTragedyIncidents BasicTragedy


initMysteryCircle : TragedySet
initMysteryCircle =
    TragedySet "Mystery Circle X" 2 initMysteryCirclePlots initMysteryCircleRoles initMysteryCircleIncidents MysteryCircle


type Error
    = NoError


decoderTragedySet : D.Decoder TragedySet
decoderTragedySet =
    D.map getTragedySetFromString D.string


getTragedySetFromString : String -> TragedySet
getTragedySetFromString s =
    if s == typeToString BasicTragedy then
        initBasicTragedy

    else
        initFirstSteps


decoder : Decoder TragedySet Error TragedySet
decoder =
    Decoder.identity


toName : TragedySet -> String
toName set =
    typeToName set.setType


typeToName : TragedySetType -> String
typeToName t =
    case t of
        BasicTragedy ->
            "Basic Tragedy χ"

        FirstSteps ->
            "First Steps"

        MysteryCircle ->
            "Mystery Circle"


toString : TragedySet -> String
toString set =
    typeToString set.setType


typeToString : TragedySetType -> String
typeToString t =
    case t of
        BasicTragedy ->
            "BasicTragedy"

        FirstSteps ->
            "FirstSteps"

        MysteryCircle ->
            "MysteryCircle"


type TragedySetType
    = BasicTragedy
    | FirstSteps
    | MysteryCircle
