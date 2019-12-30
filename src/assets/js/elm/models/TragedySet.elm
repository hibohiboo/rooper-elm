module Models.TragedySet exposing (..)

import Form.Decoder as Decoder exposing (Decoder, Validator)
import Html exposing (..)
import Json.Decode as D exposing (Value)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
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


type alias Role =
    { roleType : RoleType
    , name : String
    , limit : Maybe Int
    }



-- Role Method


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


roleFromString : String -> Maybe Role
roleFromString s =
    case s of
        "Person" ->
            Just person

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



-- 事件


type alias Incident =
    { name : String
    , effect : String
    }


murder : Incident
murder =
    Incident "殺人事件" "犯人と同一エリアにいる犯人以外の任意のキャラクター１人を死亡させる。"


increasingUnease : Incident
increasingUnease =
    Incident "不安拡大" "任意のキャラクター１人に不安カウンターを２つ置き、任意の別のキャラクター１人に暗躍カウンターを１つ置く。"


suicide : Incident
suicide =
    Incident "自殺" "犯人は死亡する。"


hospitalIncident : Incident
hospitalIncident =
    Incident "病院の事件" "病院に[暗躍カウンター]が１つ以上→病院にいる全てのキャラクターを死亡させる。病院に[暗躍カウンター]が２つ以上→主人公を死亡させる。"


farawayMurder : Incident
farawayMurder =
    Incident "遠隔殺人" "[暗躍カウンター]が２つ以上置かれたキャラクター１人を死亡させる"


missingPerson : Incident
missingPerson =
    Incident "行方不明" "犯人を任意のボードに移動させる。その後、犯人のいるボードに[暗躍カウンター]を１つ置く"


spreading : Incident
spreading =
    Incident "流布" "任意のキャラクター１人から[友好カウンター]を２つ取り除き、別のキャラクター１人に[友好カウンター]を２つ置く。"


foulEvil : Incident
foulEvil =
    Incident "邪気の汚染" "神社に[暗躍カウンター]を２つ置く。"


butterflyEffect : Incident
butterflyEffect =
    Incident "蝶の羽ばたき" "犯人と同一エリアにいるキャラクター1人にいずれかのカウンターを１つ置く。"


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


type alias TragedySet =
    { name : String
    , subPlotNumber : Int
    , plots : List Plot
    , roles : List Role
    , incidents : List Incident
    , setType : TragedySetType
    }


initBasicTragedy : TragedySet
initBasicTragedy =
    TragedySet "Basic Tragedy X" 2 initBasicPlots initBasicTragedyRoles initBasicTragedyIncidents BasicTragedy


initFirstSteps : TragedySet
initFirstSteps =
    TragedySet "First Steps" 1 initFirstStepsPlots initFirstStepsRoles initFirstStepsIncidents FirstSteps


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


type TragedySetType
    = BasicTragedy
    | FirstSteps
