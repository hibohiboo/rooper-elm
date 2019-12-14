module Models.Room.Id exposing
    ( Error(..)
    , Id
    , decoder
    , errorField
    , toString
    )

import Form.Decoder as Decoder exposing (Decoder)


type Id
    = Id String


toString : Id -> String
toString (Id str) =
    str


type Error
    = NoError


{-| Display error on input fields.
-}
errorField : Error -> List String
errorField err =
    case err of
        NoError ->
            []


decoder : Decoder String Error Id
decoder =
    Decoder.identity
        |> Decoder.map Id
