module Models.Script.IncidentScriptData exposing (..)

import Html exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.Character as Character
import Models.RoomData.OpenSheet exposing (OpenSheetIncident)
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


encodeToUdon : IncidentScriptData -> E.Value
encodeToUdon data =
    E.object
        [ ( "incident", E.string <| TragedySet.incidentToUdonString data.incident )
        , ( "day", E.int data.day )
        , ( "culprit", Character.encodeCharacter data.culprit )
        ]


assignedIncidentDays : List IncidentScriptData -> List Int
assignedIncidentDays list =
    List.map (\d -> d.day) list


assignedCulpritCharacters : List IncidentScriptData -> List Character.Character
assignedCulpritCharacters list =
    List.map (\d -> d.culprit) list


incidentToOpenSheetIncident : IncidentScriptData -> OpenSheetIncident
incidentToOpenSheetIncident { incident, day } =
    OpenSheetIncident incident day
