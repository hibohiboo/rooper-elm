module Models.RoomName exposing (..)

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
