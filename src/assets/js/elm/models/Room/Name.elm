module Models.Room.Name exposing
    ( Error(..)
    , Name
    , decoder
    , errorField
    , toString
    )

import Form.Decoder as Decoder exposing (Decoder, Validator)


type Name
    = Name String


toString : Name -> String
toString (Name str) =
    str


type Error
    = Required


{-| Display error on input fields.
-}
errorField : Error -> String
errorField err =
    case err of
        Required ->
            "ルーム名は必須です"


decoder : Decoder String Error Name
decoder =
    Decoder.identity
        |> Decoder.assert notEmpty
        |> Decoder.map Name


notEmpty : Validator String Error
notEmpty =
    Decoder.minLength Required 1
