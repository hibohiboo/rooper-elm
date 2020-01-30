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
    , castTypes : List CastType
    , effects : List Effect
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
    { effectTypes : List EffectType
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
    | IncidentsHappen -- 事件発生時
    | DayEnd -- ターン終了フェイズ
    | WritingScript -- 脚本作成時
    | CardsAreResolved -- 行動解決フェイズ
    | Always -- 常時


toCastTypeName : CastType -> String
toCastTypeName t =
    case t of
        Unkillable ->
            "不死"

        GoodwillRefusal ->
            "友好無視"

        MandatoryGoodwillRefusal ->
            "絶対友好無視"


toCastTypeColorClass : CastType -> String
toCastTypeColorClass t =
    case t of
        Unkillable ->
            "is-warning"

        GoodwillRefusal ->
            "is-info"

        MandatoryGoodwillRefusal ->
            "is-info"


toEffectTypeName : EffectType -> String
toEffectTypeName t =
    case t of
        Mandatory ->
            "強制"

        Optional ->
            "任意"

        LossCondition ->
            "敗北条件"


toEffectTypeColorClass : EffectType -> String
toEffectTypeColorClass t =
    case t of
        Mandatory ->
            "is-warning"

        Optional ->
            "is-primary"

        LossCondition ->
            "is-danger"


toEffectTimingName : Timing -> String
toEffectTimingName t =
    case t of
        WritingScript ->
            "脚本作成時"

        Always ->
            "常時"

        LoopEnd ->
            "ループ終了時"

        LoopStart ->
            "ループ開始時"

        CardsAreResolved ->
            "行動解決フェイズ"

        MastermindAbility ->
            "脚本家能力フェイズ"

        IncidentsHappen ->
            "事件フェイズ"

        DayEnd ->
            "ターン終了フェイズ"



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
    Plot "復讐者の灯火" True LightOfTheAvenger [ brain ] [ Effect [ LossCondition ] LoopEnd False "ループ終了時にボードXに暗躍カウンターが2つ以上置かれている場合、主人公は敗北する。Xはクロマクの初期エリアに等しい。" ]


aPlaceToProtect : Plot
aPlaceToProtect =
    Plot "守るべき場所" True APlaceToProtect [ keyPerson, cultist ] [ Effect [ LossCondition ] LoopEnd False "ループ終了時に学校に暗躍カウンターが2つ以上置かれている場合、主人公は敗北する。" ]


theSealedItem : Plot
theSealedItem =
    Plot "封印されしモノ" True TheSealedItem [ brain, cultist ] [ Effect [ LossCondition ] LoopEnd False "ループ終了時に神社に暗躍カウンターが2つ以上置かれている場合、主人公は敗北する。" ]


signWithMe : Plot
signWithMe =
    Plot "僕と契約しようよ！"
        True
        SignWithMe
        [ keyPerson ]
        [ Effect [ LossCondition ] LoopEnd False "ループ終了時にキーパーソンに暗躍カウンターが2つ以上置かれている場合、主人公は敗北する。"
        , Effect [ Mandatory ] WritingScript False "キーパーソンは少女にしなくてはならない"
        ]


changeOfFuture : Plot
changeOfFuture =
    Plot "未来改変プラン" True ChangeOfFuture [ timeTraveler, cultist ] [ Effect [ LossCondition ] LoopEnd False "このループ中に蝶の羽ばたきが発生していた場合、ループ終了時に主人公は敗北する。" ]


giantTimeBomb : Plot
giantTimeBomb =
    Plot "巨大時限爆弾Xの存在" True GiantTimeBomb [ witch ] [ Effect [ LossCondition ] LoopEnd False "ループ終了時にボードXに暗躍カウンターが2つ以上置かれている場合、主人公は敗北する。Xはウィッチの初期エリアに等しい。" ]


anUnsettlingRumour : Plot
anUnsettlingRumour =
    Plot "不穏な噂" False AnUnsettlingRumour [ conspiracyTheorist ] [ Effect [ Optional ] MastermindAbility True "脚本家能力フェイズに任意のボード1つに暗躍カウンターを1つ置いてもよい。" ]


aHideousScript : Plot
aHideousScript =
    Plot "最低の却本" False AHideousScript [ friend, curmudgeon, curmudgeon, conspiracyTheorist ] [ Effect [ Optional ] WritingScript False "マイナスを２人ではなく、１人や０人追加してもよい。" ]


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
    Plot "妄想拡大ウイルス" False ParanoiaVirus [ conspiracyTheorist ] [ Effect [ Mandatory ] Always False "パーソンは[不安カウンター]が３つ以上置かれている限り、役職がシリアルキラーに変更される。" ]


threadsOfFate : Plot
threadsOfFate =
    Plot "因果の糸" False ThreadsOfFate [] [ Effect [ Mandatory ] LoopStart False "ひとつ前のループ終了時に[友好カウンター]が置かれていた全キャラクターに[不安カウンター]を２つ置く。" ]


unknownFactorX : Plot
unknownFactorX =
    Plot "不定因子χ" False UnknownFactorX [ factor ] []



-- Mystery Circle


aQuiltOfIncidents : Plot
aQuiltOfIncidents =
    Plot "組み重なり事件キルト" True AQuiltOfIncidents [ fool, conspiracyTheorist ] [ Effect [ LossCondition ] LoopEnd False "ループ終了時にExゲージが3以上の場合、主人公は敗北する。" ]


tightropePlan : Plot
tightropePlan =
    Plot "タイトロープ上の計画" True TightropePlan [ killer, brain ] [ Effect [ LossCondition ] LoopEnd False "ループ終了時にExゲージが1以下の場合、主人公は敗北する。" ]


theBlackSchool : Plot
theBlackSchool =
    Plot "黒の学園" True TheBlackSchool [ brain ] [ Effect [ LossCondition ] LoopEnd False "ループ終了時に学校に暗躍カウンターがX個以上置かれている場合、主人公は敗北する。Xは現在のループ回数に等しい。" ]


aDropOfStrychnine : Plot
aDropOfStrychnine =
    Plot "ストリキニーネの雫" True ADropOfStrychnine [ keyPerson, poisoner, fool ] [ Effect [ Mandatory ] IncidentsHappen False "「殺人事件」「自殺」が発生するか判定するとき、暗躍カウンターを不安カウンターとしても扱う。" ]


isolatedInstitutionPsycho : Plot
isolatedInstitutionPsycho =
    Plot "隔離病棟サイコ" False IsolatedInstitutionPsycho [ conspiracyTheorist, paranoiac, therapist ] [ Effect [ Mandatory ] LoopStart False "各ループの開始時、ひとつ前のループの終了時にExゲージが2以下であった場合、Exゲージを1増加させる。" ]


smellOfGunpowder : Plot
smellOfGunpowder =
    Plot "火薬の香り" False SmellOfGunpowder [ serialKiller ] [ Effect [ LossCondition ] LoopEnd False "ループ終了時に、生存している全キャラクターの上に置かれた不安カウンターの合計が12以上の場合、主人公は敗北する。" ]


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
    , theHiddenFreak
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
    Role Person "パーソン" Nothing [] []


killer : Role
killer =
    Role Killer "キラー" Nothing [ GoodwillRefusal ] killerEffects


killerEffects : List Effect
killerEffects =
    [ Effect [ Optional ] MastermindAbility False "キーパーソンに2つ以上の暗躍カウンターが置かれており、このキャラクターが同一のエリアにいる場合、ターン終了フェイズにキーパーソンを死亡させても良い。"
    , Effect [ Optional, LossCondition ] MastermindAbility False "このキャラクターに4つ以上の暗躍カウンターが置かれている場合、ターン終了フェイズに主人公を死亡させても良い。"
    ]


brain : Role
brain =
    Role Brain "クロマク" Nothing [ GoodwillRefusal ] [ Effect [ Optional ] MastermindAbility False "各ターンの脚本家能力フェイズにこのキャラクターと同一のエリアにいるキャラクター1人か、このキャラクターのいるボードに暗躍カウンターを1つ置いても良い。" ]


keyPerson : Role
keyPerson =
    Role KeyPerson "キーパーソン" Nothing [] [ Effect [ Mandatory, LossCondition ] Always False "このキャラクターが死亡した時、主人公は敗北し、直ちにこのループを終了させる。" ]


cultist : Role
cultist =
    Role Cultist "カルティスト" Nothing [ MandatoryGoodwillRefusal ] [ Effect [ Optional ] CardsAreResolved False "各ターンの行動解決フェイズに、このキャラクターと同一のエリアにいるキャラクター、ならびにこのキャラクターのいるボードにセットされた暗躍禁止は無視しても良い。" ]


timeTraveler : Role
timeTraveler =
    Role TimeTraveler "タイムトラベラー" Nothing [ Unkillable ] timeTravelerEffects


timeTravelerEffects : List Effect
timeTravelerEffects =
    [ Effect [ Mandatory ] CardsAreResolved False "このキャラクターにセットされた友好禁止は無視される。"
    , Effect [ Optional ] DayEnd False "最終日のターン終了フェイズに、このキャラクターに2つ以下の友好カウンターしか置かれていない場合、主人公を敗北させてもよい。そうした場合、直ちにこのループを終了させる。"
    ]


witch : Role
witch =
    Role Witch "ウィッチ" Nothing [ MandatoryGoodwillRefusal ] []


friend : Role
friend =
    Role Friend "フレンド" (Just 2) [] friendEffects


friendEffects =
    [ Effect [ Mandatory, LossCondition ] LoopEnd False "このカードがループ終了時に死亡している場合、このカードの役職を公開し、主人公は敗北する。"
    , Effect [ Mandatory ] LoopStart False "このキャラクターの役職が公開されたことがある場合、ループ開始時にこのキャラクターに有効カウンターを１つ置く"
    ]


lovedOne : Role
lovedOne =
    Role LovedOne "メインラバーズ" Nothing [] lovedOneEffects


lovedOneEffects : List Effect
lovedOneEffects =
    [ Effect [ Mandatory ] Always False "ラバーズが死亡した時、このキャラクターに不安カウンターを6つ置く"
    , Effect [ Optional, LossCondition ] DayEnd False "このキャラクターに3つ以上の不安カウンターと1つ以上の暗躍カウンターが置かれている場合、ターン終了フェイズに主人公を死亡させても良い。"
    ]


lover : Role
lover =
    Role Lover "ラバーズ" Nothing [] [ Effect [ Mandatory ] Always False "メインラバーズが死亡した時、このキャラクターに不安カウンターを6つ置く" ]


serialKiller : Role
serialKiller =
    Role SerialKiller "シリアルキラー" Nothing [] [ Effect [ Mandatory ] DayEnd False "このキャラクターと同一エリアにいるキャラクターが1人だけの場合、ターン終了フェイズにそのキャラクターを死亡させる。" ]


factor : Role
factor =
    Role Factor "ファクター" Nothing [ GoodwillRefusal ] factorEffects


factorEffects : List Effect
factorEffects =
    [ Effect [ Mandatory ] Always False "学校に暗躍カウンターが2つ以上置かれている場合、このキャラクターはミスリーダーに記載された追加能力を得る。"
    , Effect [ Mandatory, LossCondition ] Always False "都市に暗躍カウンターが2つ以上置かれている場合、このキャラクターはキーパーソンに記載された追加能力を得る。"
    ]


conspiracyTheorist : Role
conspiracyTheorist =
    Role ConspiracyTheorist "ミスリーダー" (Just 1) [] [ Effect [ Optional ] MastermindAbility False "各ターンの脚本家能力フェイズにこのキャラクターと同一エリアにいるキャラクター1人に不安カウンターを1つ置いても良い。" ]


curmudgeon : Role
curmudgeon =
    Role Curmudgeon "マイナス" Nothing [ GoodwillRefusal ] []


poisoner : Role
poisoner =
    Role Poisoner "ドリッパー" Nothing [ GoodwillRefusal ] poisonerEffects


poisonerEffects : List Effect
poisonerEffects =
    [ Effect [ Mandatory ] DayEnd True "Exゲージが2以上の場合、ターン終了フェイズにこのキャラクターと同一のエリアにいるキャラクター1人を死亡させる。(1ループに1回まで)"
    , Effect [ Mandatory, LossCondition ] DayEnd False "EXゲージが4以上の場合、ターン終了フェイズに主人公を死亡させる。"
    ]


fool : Role
fool =
    Role Fool "フール" (Just 1) [] foolEffects


foolEffects : List Effect
foolEffects =
    [ Effect [ Mandatory ] WritingScript False "このキャラクターは必ずいずれかの事件の犯人となる。"
    , Effect [ Mandatory ] Always False "このキャラクターが事件を発生させた場合、その事件の解決後にこのカードから全ての不安カウンターを取り除く。"
    ]


paranoiac : Role
paranoiac =
    Role Paranoiac "パラノイア" Nothing [ MandatoryGoodwillRefusal ] [ Effect [ Optional ] MastermindAbility False "各ターンの脚本家能力フェイズにこのキャラクターに暗躍カウンターか不安カウンターを1つ置いても良い。" ]


therapist : Role
therapist =
    Role Therapist "セラピスト" Nothing [] [ Effect [ Mandatory ] MastermindAbility False "Exゲージが1以上の場合、脚本家能力フェイズに、このキャラクターと同一エリアにいる自身以外のキャラクター1人の不安カウンターを1つ取り除く。" ]


privateInvestigator : Role
privateInvestigator =
    Role PrivateInvestigator "メイタンテイ" Nothing [ Unkillable ] privateInvestigatorEffects


privateInvestigatorEffects : List Effect
privateInvestigatorEffects =
    [ Effect [ Mandatory ] WritingScript False "このキャラクターは犯人にならない"
    , Effect [ Mandatory ] IncidentsHappen False "Exゲージが0である、かつこのキャラクターと、同一エリアにいるキャラクターが犯人である事件の発生を判定する場合、犯人に置かれた不安カウンターの個数によらず、犯人が生存していれば必ず事件が発生する。"
    ]


twin : Role
twin =
    Role Twin "ツイン" Nothing [] twinEffects


twinEffects : List Effect
twinEffects =
    [ Effect [ Mandatory ] WritingScript False "このキャラクターは必ずいずれかの事件の犯人となる。"
    , Effect [ Mandatory ] IncidentsHappen False "このキャラクターが事件を発生させる場合、このキャラクターは本来の位置の代わりに、本来の対角線にあるボード上にいるものとして扱う。"
    ]


obstinate : Role
obstinate =
    Role Obstinate "ゼッタイシャ" Nothing [ MandatoryGoodwillRefusal ] obstinateEffects


obstinateEffects : List Effect
obstinateEffects =
    [ Effect [ Mandatory ] WritingScript False "このキャラクターは必ずいずれかの事件の犯人となる"
    , Effect [ Mandatory ] IncidentsHappen False "このキャラクターはその上に置かれた不安カウンターの個数によらず、生存していれば必ず事件を発生させる。"
    ]


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
    Incident SerialMurder "連続殺人" "犯人と同一エリアにいる犯人以外の任意のキャラクター1人を死亡させる。脚本に複数の連続殺人がある場合、あるキャラクターが複数の連続殺人の事件の犯人となってもよい。"


fakedSuicide : Incident
fakedSuicide =
    Incident FakedSuicide "偽装自殺" "犯人にExカードをセットする。（このループ中そのキャラクターにはExカードAをつけたままにしておく。別の偽装自殺が起きたら付け替える）ExカードAのセットされたキャラクターに対し、主人公はカードをセットできない。"


terrorism : Incident
terrorism =
    Incident Terrorism "テロリズム" "都市に暗躍カウンターが1つ以上置かれている場合、都市にいるキャラクター全員が死亡する。さらに都市に暗躍カウンターが2つ以上置かれている場合、主人公は死亡する。"


portent : Incident
portent =
    Incident Portent "前兆" "犯人と同じエリアにいる任意のキャラクター1人に不安カウンターを1つ乗せる。この事件は犯人の不安臨界を1少ないものとして発生するかを判定する。"


bestialMurder : Incident
bestialMurder =
    Incident BestialMurder "猟奇殺人" "「殺人事件」と「不安拡大」をこの順番で発生させる。この事件の発生によりExゲージは増加しない。（結果としてExゲージは２つ増加する）この事件は犯人の不安臨界を1多いものとして発生するかを判定する。"


aSuspiciousLetter : Incident
aSuspiciousLetter =
    Incident ASuspiciousLetter "不審な手紙" "犯人と同一エリアにいるキャラクター1人を任意のボードに移動させる。次の日の間、そのキャラクターは移動できない。"


closedCircle : Incident
closedCircle =
    Incident ClosedCircle "クローズドサークル" "犯人のいるボードを指定する。事件が発生した日から発生した日を含む3日間、そのボードへの移動とそのボードからの移動が行われる場合、それは代わりに行われない。"


theSilverBullet : Incident
theSilverBullet =
    Incident TheSilverBullet "銀の銃弾" "このフェイズの終了時にループを終了させる。この事件の発生によりExゲージは増加しない。（この事件によりループが終了した時点で、主人公が敗北条件を満たしていない場合は、主人公プレイヤーの勝利としてゲームが終了する。）"


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
    , aSuspiciousLetter
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
    D.map tragedySetFromString D.string


tragedySetFromString : String -> TragedySet
tragedySetFromString s =
    case typeFromString s of
        Just BasicTragedy ->
            initBasicTragedy

        Just MysteryCircle ->
            initMysteryCircle

        Just FirstSteps ->
            initFirstSteps

        Nothing ->
            initBasicTragedy


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


typeFromString : String -> Maybe TragedySetType
typeFromString s =
    case s of
        "BasicTragedy" ->
            Just BasicTragedy

        "FirstSteps" ->
            Just FirstSteps

        "MysteryCircle" ->
            Just MysteryCircle

        _ ->
            Nothing


type TragedySetType
    = BasicTragedy
    | FirstSteps
    | MysteryCircle
