module Models.RoomData exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Json.Encode.Extra as ExEncode
import Models.Room as Room exposing (Room)
import Models.RoomData.OpenSheet as OpenSheet exposing (OpenSheet)
import Models.RoomData.Protagonist as Protagonist exposing (Protagonist)
import Models.Script as Script exposing (Script)
import Models.User exposing (User)


type alias RoomData =
    { id : String
    , protagonists : List Protagonist
    , openSheet : OpenSheet
    , script : Maybe Script
    }


init : Maybe RoomData
init =
    Nothing


initRoomData : Room -> RoomData
initRoomData room =
    let
        protagonists =
            Protagonist.init room.protagonist1TwitterScreenName room.protagonist2TwitterScreenName room.protagonist3TwitterScreenName
    in
    RoomData (Room.getId room) protagonists (Script.scriptToOpenSheet room.script) Nothing



-- Method


isRoomOwner : Maybe RoomData -> Bool
isRoomOwner room =
    case room of
        Just _ ->
            True

        Nothing ->
            False


isRoomMember : RoomData -> User -> Bool
isRoomMember data user =
    Protagonist.isProtagonist user.twitterScreenName data.protagonists



-- ==============================================================================================
-- setter
-- ==============================================================================================


setScript : Maybe Script -> RoomData -> RoomData
setScript s f =
    { f | script = s }



-- ==============================================================================================
-- デコーダ
-- ==============================================================================================


decode : D.Value -> Maybe RoomData
decode json =
    D.decodeValue decoder json
        |> Result.toMaybe


decoder : D.Decoder RoomData
decoder =
    D.succeed RoomData
        |> Pipeline.required "id" D.string
        |> Pipeline.required "protagonists" (D.list Protagonist.decoder)
        |> Pipeline.required "openSheet" OpenSheet.decoder
        |> Pipeline.optional "script" Script.scriptDecoder Nothing



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : RoomData -> E.Value
encode data =
    E.object
        [ ( "id", E.string data.id )
        , ( "protagonists", E.list Protagonist.encode data.protagonists )
        , ( "openSheet", OpenSheet.encode data.openSheet )
        , ( "script", ExEncode.maybe Script.encode data.script )
        ]



-- ==============================================================================================
-- View
-- ==============================================================================================


openSheet : RoomData -> Html msg
openSheet data =
    data.openSheet
        |> OpenSheet.openSheetView


closeSheet : RoomData -> Html msg
closeSheet data =
    case data.script of
        Just script ->
            Script.closeSheet script

        Nothing ->
            text ""


tags : Maybe RoomData -> User -> Html msg
tags data user =
    div []
        (List.concat
            [ [ if isRoomOwner data then
                    span [ class "tag is-danger" ] [ text "脚本家" ]

                else
                    text ""
              ]
            , case data of
                Nothing ->
                    []

                Just d ->
                    d.protagonists
                        |> Protagonist.getUserProtagonists user.twitterScreenName
                        |> List.map
                            (\p ->
                                span
                                    [ class <|
                                        if p.number == 1 then
                                            "tag is-info"

                                        else if p.number == 2 then
                                            "tag is-warning"

                                        else
                                            "tag is-success"
                                    ]
                                    [ text p.name ]
                            )
            ]
        )
