module Main exposing (keyedRoom, room, roomField, rooms)

import Html.Attributes as Attributes exposing (..)
import Html.Keyed as Keyed
import Html.Lazy as Html
import Models.RoomName as RoomName exposing (RoomName)


rooms : List RoomName -> Html msg
rooms rs =
    div []
        [ Keyed.node "div"
            []
          <|
            List.map keyedRoom rs
        ]


keyedRoom : RoomName -> ( String, Html msg )
keyedRoom r =
    ( Name.toString r.name, Html.lazy room r )


room : RoomName -> Html msg
room r =
    div
        [ class "roomWrapper"
        ]
        [ div
            [ class "room"
            ]
            [ roomField "Name" r.name
            , roomField "Id" r.id
            ]
        ]


roomField : String -> String -> Html msg
roomField title content =
    div
        [ class "roomField"
        ]
        [ div
            [ class "roomTitle"
            ]
            [ text title
            ]
        , div
            [ class "roomContent"
            ]
            [ text content
            ]
        ]
