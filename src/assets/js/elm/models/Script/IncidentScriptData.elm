module Models.Script.IncidentScriptData exposing (..)

import Form.Decoder as Decoder exposing (Decoder, Validator)
import Html exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.Character as Character
import Models.TragedySet as TragedySet


type alias IncidentScriptData =
    { incident : TragedySet.Incident
    , day : Int
    , culprit : Character.Character
    }


decode : D.Decoder IncidentScriptData
decode =
    D.succeed IncidentScriptData
        |> Pipeline.required "incident" (D.map (TragedySet.incidentFromString >> Maybe.withDefault TragedySet.murder) D.string)
        |> Pipeline.required "day" D.int
        |> Pipeline.required "culprit" Character.decoderCharacter


encode : IncidentScriptData -> E.Value
encode data =
    E.object
        [ ( "incident", E.string <| TragedySet.incidentToString data.incident )
        , ( "day", E.int data.day )
        , ( "culprit", Character.encodeCharacter data.culprit )
        ]
