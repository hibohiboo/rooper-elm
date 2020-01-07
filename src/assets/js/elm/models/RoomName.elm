module Models.RoomName exposing (..)

import Html exposing (..)
import Html.Attributes as Attributes exposing (class, href)
import Html.Keyed as Keyed
import Html.Lazy as Html
import Json.Decode as D exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (optional, required)


type alias RoomName =
    { id : String
    , name : String
    , scriptId : String
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
        |> optional "scriptId" D.string ""


roomEdits : List RoomName -> Html msg
roomEdits rs =
    Keyed.node "div"
        [ class "panel" ]
    <|
        roomEditsTitle
            :: List.map keyedRoomEdit rs


roomEditsTitle : ( String, Html msg )
roomEditsTitle =
    ( "room-edits-title", p [ class "panel-heading" ] [ text "ルーム編集" ] )


keyedRoomEdit : RoomName -> ( String, Html msg )
keyedRoomEdit r =
    ( r.id, Html.lazy roomEdit r )


roomEdit : RoomName -> Html msg
roomEdit r =
    a
        [ class "panel-block"
        , href ("/rooper/room/edit/" ++ r.id ++ "/")
        ]
        [ span [ class "panel-icon" ] [ i [ class "fa fa-edit" ] [] ]
        , text r.name

        -- , span [ class "tag is-info" ] [ span [ class "panel-icon" ] [ i [ class "fa fa-edit" ] [] ], span [] [ text "編集" ] ]
        ]


rooms : List RoomName -> Html msg
rooms rs =
    let
        list =
            List.filter (\r -> r.scriptId /= "") rs
    in
    case list of
        [] ->
            text ""

        _ ->
            Keyed.node "div"
                [ class "panel" ]
            <|
                roomsTitle
                    :: List.map keyedRoom list


roomsTitle : ( String, Html msg )
roomsTitle =
    ( "rooms-title", p [ class "panel-heading" ] [ text "ルーム入室" ] )


keyedRoom : RoomName -> ( String, Html msg )
keyedRoom r =
    ( r.id, Html.lazy room r )


room : RoomName -> Html msg
room r =
    a
        [ class "panel-block"
        , href ("/rooper/room/" ++ r.id ++ "/")
        ]
        [ span [ class "panel-icon" ] [ i [ class "fas fa-angle-right" ] [] ]
        , text r.name
        ]
