module Models.TragedySet exposing (..)

import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Json.Decode as D exposing (Value)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E


initBasicTragedy : TragedySet
initBasicTragedy =
    TragedySet "Basic Tragedy X" 2


type alias TragedySet =
    { name : String
    , subPlotNumber : Int
    }


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


killer : Role
killer =
    Role "キラー" Nothing


brain : Role
brain =
    Role "クロマク" Nothing


keyPerson : Role
keyPerson =
    Role "キーパーソン" Nothing


cultist : Role
cultist =
    Role "カルティスト" Nothing


timeTraveler : Role
timeTraveler =
    Role "タイムトラベラー" Nothing


witch : Role
witch =
    Role "ウィッチ" Nothing


friend : Role
friend =
    Role "フレンド" (Just 2)


lovedOne : Role
lovedOne =
    Role "メインラバーズ" Nothing


lover : Role
lover =
    Role "ラバーズ" Nothing


serialKiller : Role
serialKiller =
    Role "シリアルキラー" Nothing


factor : Role
factor =
    Role "ファクター" Nothing


conspiracyTheorist : Role
conspiracyTheorist =
    Role "ミスリーダー" (Just 1)


type alias Role =
    { name : String
    , limit : Maybe Int
    }


type alias Plot =
    { name : String
    , plotType : PlotType
    , roles : List Role
    , effects : List Effect
    }


type PlotType
    = MainPlot
    | SubPlot


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


murderPlan : Plot
murderPlan =
    Plot "殺人計画" MainPlot [ killer, brain, keyPerson ] []


theSealedItem : Plot
theSealedItem =
    Plot "封印されしモノ" MainPlot [ brain, cultist ] [ Effect LossCondition LoopEnd False "神社に[暗躍カウンター]が２つ以上ある。" ]


signWithMe : Plot
signWithMe =
    Plot "僕と契約しようよ！"
        MainPlot
        [ keyPerson ]
        [ Effect LossCondition LoopEnd False "キーパーソンに[暗躍カウンター]が２個以上。"
        , Effect Mandatory WritingScript False "キーパーソンは少女にしなくてはならない"
        ]


changeOfFuture : Plot
changeOfFuture =
    Plot "未来改変プラン" MainPlot [ timeTraveler, cultist ] [ Effect LossCondition LoopEnd False "このループ中に「事件：蝶の羽ばたき」が発生している。" ]


giantTimeBomb : Plot
giantTimeBomb =
    Plot "巨大時限爆弾Xの存在" MainPlot [ witch ] [ Effect LossCondition LoopEnd False "ウィッチの初期エリアに[暗躍カウンター]が２つ以上。" ]


initBasicPlots : List Plot
initBasicPlots =
    [ murderPlan, theSealedItem, signWithMe, changeOfFuture, giantTimeBomb ]
