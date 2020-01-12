module Models.RoomData exposing (..)

import Html exposing (..)
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
            Protagonist.init 1 room.protagonist1TwitterScreenName
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    RoomData (Room.getId room) protagonists (Script.scriptToOpenSheet room.script) Nothing



-- Method


isRoomMember : RoomData -> User -> Bool
isRoomMember data user =
    if (List.filter (\d -> d.twitterScreenName == user.twitterScreenName) data.protagonists |> List.length) > 0 then
        True

    else
        False



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
