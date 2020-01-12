module Models.RoomData exposing (..)

import Html exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.Room as Room exposing (Room)
import Models.RoomData.OpenSheet as OpenSheet exposing (OpenSheet)
import Models.Script as Script
import Models.User exposing (User)


type alias RoomData =
    { id : String
    , protagonist1TwitterScreenName : String
    , openSheet : OpenSheet
    }


init : Maybe RoomData
init =
    Nothing


initRoomData : Room -> RoomData
initRoomData room =
    RoomData (Room.getId room) room.protagonist1TwitterScreenName (Script.scriptToOpenSheet room.script)


isRoomMember : RoomData -> User -> Bool
isRoomMember data user =
    if user.twitterScreenName == data.protagonist1TwitterScreenName then
        True

    else
        False



-- Method
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
        |> Pipeline.required "protagonist1TwitterScreenName" D.string
        |> Pipeline.required "openSheet" OpenSheet.decoder



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : RoomData -> E.Value
encode data =
    E.object
        [ ( "id", E.string data.id )
        , ( "protagonist1TwitterScreenName", E.string data.id )
        , ( "openSheet", OpenSheet.encode data.openSheet )
        ]



-- ==============================================================================================
-- View
-- ==============================================================================================


openSheet : RoomData -> Html msg
openSheet data =
    data.openSheet
        |> OpenSheet.openSheetView
