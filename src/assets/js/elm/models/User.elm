module Models.User exposing (User, decodeUserFromJson)

import Json.Decode as Decode exposing (Decoder, Value, string, succeed)
import Json.Decode.Pipeline exposing (required)


type alias User =
    { uid : String
    , displayName : String
    , storeUserId : String
    , twitterScreenName : String
    , twitterProfileImageUrl : String
    }


decodeUserFromJson : Value -> Maybe User
decodeUserFromJson json =
    -- let
    --     _ =
    --         Debug.log "decodeUser" json
    -- in
    json
        |> Decode.decodeValue decoder
        |> Result.toMaybe


decoder : Decoder User
decoder =
    Decode.succeed User
        |> Json.Decode.Pipeline.required "uid" Decode.string
        |> Json.Decode.Pipeline.required "displayName" Decode.string
        |> Json.Decode.Pipeline.required "storeUserId" Decode.string
        |> Json.Decode.Pipeline.required "twitterScreenName" Decode.string
        |> Json.Decode.Pipeline.required "twitterProfileImageUrl" Decode.string
