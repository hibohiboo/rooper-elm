module Views.RoomName exposing (keyedRoom, room, rooms)

import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Keyed as Keyed
import Html.Lazy as Html
import Models.RoomName as RoomName exposing (RoomName)


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
