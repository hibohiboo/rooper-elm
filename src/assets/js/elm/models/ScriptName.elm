module Models.ScriptName exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Keyed as Keyed
import Html.Lazy as Html
import Json.Decode as D exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)


type alias ScriptName =
    { id : String
    , name : String
    }


initScriptNames : Maybe (List ScriptName)
initScriptNames =
    Nothing


decodeScriptNameListFromJson : Value -> Maybe (List ScriptName)
decodeScriptNameListFromJson json =
    json
        |> D.decodeValue (D.list decoder)
        |> Result.toMaybe


decoder : Decoder ScriptName
decoder =
    D.succeed ScriptName
        |> required "id" D.string
        |> required "name" D.string


scripts : List ScriptName -> Html msg
scripts rs =
    Keyed.node "div"
        [ class "panel" ]
    <|
        scriptsTitle
            :: List.map keyedScript rs


scriptsTitle : ( String, Html msg )
scriptsTitle =
    ( "scripts-title", p [ class "panel-heading" ] [ text "シナリオ" ] )


keyedScript : ScriptName -> ( String, Html msg )
keyedScript r =
    ( r.id, Html.lazy script r )


script : ScriptName -> Html msg
script r =
    a
        [ class "panel-block"
        , href ("/rooper/script/edit/" ++ r.id)
        ]
        [ text r.name
        ]
