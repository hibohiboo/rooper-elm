module Models.RoomData.OpenSheet exposing (..)

import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.TragedySet as TragedySet exposing (Effect, Incident, Plot, Role, TragedySet)


type alias OpenSheetIncident =
    { incident : Incident
    , day : Int
    }


type alias OpenSheet =
    { set : TragedySet
    , numberOfLoops : Int
    , daysInOneLoop : Int
    , incidents : List OpenSheetIncident
    , extra : String
    }



-- ==============================================================================================
-- デコーダ
-- ==============================================================================================


decoder : D.Decoder OpenSheet
decoder =
    D.succeed OpenSheet
        |> Pipeline.optional "set" TragedySet.decoderTragedySet TragedySet.initBasicTragedy
        |> Pipeline.optional "numberOfLoops" D.int 1
        |> Pipeline.optional "daysInOneLoop" D.int 1
        |> Pipeline.optional "incidents" (D.list incidentDecoder) []
        |> Pipeline.optional "extra" D.string ""


incidentDecoder : D.Decoder OpenSheetIncident
incidentDecoder =
    D.succeed OpenSheetIncident
        |> Pipeline.required "incident" (D.map (TragedySet.incidentFromString >> Maybe.withDefault TragedySet.murder) D.string)
        |> Pipeline.required "day" D.int



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : OpenSheet -> E.Value
encode script =
    E.object
        [ ( "set", E.string <| TragedySet.toString script.set )
        , ( "numberOfLoops", E.int script.numberOfLoops )
        , ( "daysInOneLoop", E.int script.daysInOneLoop )
        , ( "incidents", E.list incidentEncode script.incidents )
        , ( "extra", E.string script.extra )
        ]


incidentEncode : OpenSheetIncident -> E.Value
incidentEncode data =
    E.object
        [ ( "incident", E.string <| TragedySet.incidentToString data.incident )
        , ( "day", E.int data.day )
        ]



-- ==============================================================================================
-- View
-- ==============================================================================================


openSheetView : OpenSheet -> Html msg
openSheetView s =
    div []
        [ openSheetViewDetail s
        , tragedySetView s.set
        ]


openSheetViewDetail : OpenSheet -> Html msg
openSheetViewDetail s =
    div [ class "box" ]
        [ div [ class "title is-5" ]
            [ text "公開シート"
            ]
        , Form.field
            [ label [ class "label has-text-white" ]
                [ text "使用セット"
                ]
            , div []
                [ text <| TragedySet.toName s.set
                ]
            ]
        , Form.field
            [ label [ class "label has-text-white" ] [ text "ループ回数" ]
            , div []
                [ text <| String.fromInt s.numberOfLoops
                ]
            ]
        , Form.field
            [ label [ class "label has-text-white" ] [ text "１ループ日数" ]
            , div []
                [ text <| String.fromInt s.daysInOneLoop
                ]
            ]
        , Form.field
            [ label [ class "label has-text-white" ] [ text "特別ルール" ]
            , Form.control
                [ div [ style "white-space" "pre-wrap" ] [ text s.extra ]
                ]
            ]
        , Form.field [ label [ class "label" ] [ text "事件" ] ]
        , Form.field <|
            (s.incidents
                |> List.sortBy .day
                |> List.map
                    (\data ->
                        div [ class "media" ]
                            [ div [ class "media-left", style "padding-left" "1rem", style "align-self" "center" ]
                                [ text <| String.fromInt data.day ++ "日目"
                                ]
                            , div [ class "media-content is-flex" ]
                                [ div [ style "text-align" "center", style "align-self" "center" ] [ text data.incident.name ] ]
                            ]
                    )
            )
        ]


incidentIcon : Int -> List OpenSheetIncident -> Html msg
incidentIcon date list =
    let
        incidentDays =
            List.map (\i -> i.day) list
    in
    if List.member date incidentDays then
        text "有"

    else
        text ""


tragedySetView : TragedySet -> Html msg
tragedySetView set =
    div [ class "box" ]
        [ div [ class "content" ]
            (h3 [ class "title" ] [ text "ルールY" ]
                :: (List.map (\p -> plotView p) <| TragedySet.filterMainPlots <| set.plots)
            )
        , div [ class "content" ]
            (h3 [ class "title" ] [ text "ルールX" ]
                :: (List.map (\p -> plotView p) <| TragedySet.filterSubPlots <| set.plots)
            )
        , div [ class "content" ]
            (h3 [ class "title" ] [ text "役職" ]
                :: (List.map (\p -> roleView p) <| set.roles)
            )
        ]


roleView : Role -> Html msg
roleView r =
    div [ class "card" ]
        [ header [ class "card-header" ]
            [ p [ class "card-header-title" ]
                [ text r.name
                ]
            ]
        , div [ class "card-content" ]
            [ div [ class "content" ]
                [ div
                    []
                    [ div [ class "tag is-primary" ]
                        [ text "追加能力"
                        ]
                    ]

                -- , div [] (List.map (\e -> effectView e) plot.effects)
                ]
            ]
        ]


plotView : Plot -> Html msg
plotView plot =
    div [ class "card" ]
        [ header [ class "card-header" ]
            [ p [ class "card-header-title" ]
                [ text plot.name
                ]
            ]
        , div [ class "card-content" ]
            [ div [ class "content" ]
                [ if List.length plot.roles == 0 then
                    text ""

                  else
                    div []
                        [ div [ class "tag is-primary" ]
                            [ text "役職追加"
                            ]
                        ]
                , ul [] (List.map (\r -> li [] [ text r.name ]) plot.roles)
                , if List.length plot.effects == 0 then
                    text ""

                  else
                    div []
                        [ div [ class "tag is-primary" ]
                            [ text "ルール追加"
                            ]
                        ]
                , div [] (List.map (\e -> effectView e) plot.effects)
                ]
            ]
        ]


effectView : Effect -> Html msg
effectView e =
    div []
        [ div [ class "tags has-addons", style "margin" "1.5rem 0 0 0.5rem" ]
            (span [ class "tag is-info" ] [ text <| TragedySet.toEffectTimingName e.timing ]
                :: List.map effectTypeTag e.effectTypes
            )
        , div [] [ text e.effect ]
        ]


effectTypeTag : TragedySet.EffectType -> Html msg
effectTypeTag t =
    span [ class "tag", class <| TragedySet.toEffectTypeColorClass t ] [ text <| TragedySet.toEffectTypeName t ]
