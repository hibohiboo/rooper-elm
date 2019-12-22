module Models.ScenarioName exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Keyed as Keyed
import Html.Lazy as Html
import Json.Decode as D exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)


type alias ScenarioName =
    { id : String
    , name : String
    }


initScenarioNames : Maybe (List ScenarioName)
initScenarioNames =
    Nothing


decodeScenarioNameListFromJson : Value -> Maybe (List ScenarioName)
decodeScenarioNameListFromJson json =
    json
        |> D.decodeValue (D.list decoder)
        |> Result.toMaybe


decoder : Decoder ScenarioName
decoder =
    D.succeed ScenarioName
        |> required "id" D.string
        |> required "name" D.string


scenarios : List ScenarioName -> Html msg
scenarios rs =
    Keyed.node "div"
        [ class "panel" ]
    <|
        scenariosTitle
            :: List.map keyedScenario rs


scenariosTitle : ( String, Html msg )
scenariosTitle =
    ( "scenarios-title", p [ class "panel-heading" ] [ text "シナリオ" ] )


keyedScenario : ScenarioName -> ( String, Html msg )
keyedScenario r =
    ( r.id, Html.lazy scenario r )


scenario : ScenarioName -> Html msg
scenario r =
    a
        [ class "panel-block"
        , href ("/rooper/scenario/edit/" ++ r.id)
        ]
        [ text r.name
        ]
