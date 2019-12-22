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


initRoomNames : Maybe (List RoomName)
initRoomNames =
    Nothing


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
        roomsTitle
            :: List.map keyedRoom rs


roomsTitle : ( String, Html msg )
roomsTitle =
    ( "rooms-title", p [ class "panel-heading" ] [ text "プレイルーム" ] )


keyedRoom : RoomName -> ( String, Html msg )
keyedRoom r =
    ( r.id, Html.lazy room r )


room : RoomName -> Html msg
room r =
    a
        [ class "panel-block"

        -- TODO
        -- , href ("/rooper/room/" ++ r.id)
        ]
        [ text r.name
        ]
