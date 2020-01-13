module Models.RoomData.OpenSheet exposing (..)

import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.TragedySet as TragedySet exposing (Incident, TragedySet)


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
