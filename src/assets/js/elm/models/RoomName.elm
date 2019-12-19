module Models.RoomName exposing (..)

import Html exposing (..)
import Html.Attributes as Attributes exposing (class, href)
import Html.Keyed as Keyed
import Html.Lazy as Html
import Json.Decode as D exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)


type alias RoomName =
    { id : String
    , name : String
    }


decodeRoomNameListFromJson : Value -> Maybe (List RoomName)
decodeRoomNameListFromJson json =
    -- let
    --     _ =
    --         Debug.log "decodeRoomName" json
    -- in
    json
        |> D.decodeValue (D.list decoder)
        |> Result.toMaybe


decoder : Decoder RoomName
decoder =
    D.succeed RoomName
        |> required "id" D.string
        |> required "name" D.string


rooms : List RoomName -> Html msg
rooms rs =
    Keyed.node "div"
        [ class "panel" ]
    <|
        ( "rooms-title", p [ class "panel-heading" ] [ text "プレイルーム" ] )
            :: List.map keyedRoom rs


keyedRoom : RoomName -> ( String, Html msg )
keyedRoom r =
    ( r.id, Html.lazy room r )


room : RoomName -> Html msg
room r =
    a
        [ class "panel-block"
        , href ("/rooper/room/" ++ r.id)
        ]
        [ text r.name
        ]
