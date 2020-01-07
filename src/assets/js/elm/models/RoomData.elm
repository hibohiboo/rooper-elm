module Models.RoomData exposing (..)

import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E


type alias RoomData =
    { id : Id
    , name : Name
    , scriptId : String
    , mastermindTwitterScreenName : String
    , protagonist1TwitterScreenName : String
    , protagonist2TwitterScreenName : String
    , protagonist3TwitterScreenName : String
    }
