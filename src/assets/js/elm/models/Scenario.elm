module Models.Scenario exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Keyed as Keyed
import Html.Lazy as Html
import Json.Decode as D exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)


type alias Scenario =
    { id : String
    , name : String
    }


decodeScenarioListFromJson : Value -> Maybe (List Scenario)
decodeScenarioListFromJson json =
    json
        |> D.decodeValue (D.list decoder)
        |> Result.toMaybe


decoder : Decoder Scenario
decoder =
    D.succeed Scenario
        |> required "id" D.string
        |> required "name" D.string


rooms : List Scenario -> Html msg
rooms rs =
    Keyed.node "div"
        [ class "panel" ]
    <|
        roomsTitle
            :: List.map keyedRoom rs


roomsTitle : ( String, Html msg )
roomsTitle =
    ( "rooms-title", p [ class "panel-heading" ] [ text "シナリオ一覧" ] )


keyedRoom : Scenario -> ( String, Html msg )
keyedRoom r =
    ( r.id, Html.lazy room r )


room : Scenario -> Html msg
room r =
    a
        [ class "panel-block"
        , href ("/rooper/room/" ++ r.id)
        ]
        [ text r.name
        ]
