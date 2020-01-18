module Models.Script.Name exposing
    ( Error(..)
    , Name
    , decoder
    , errorField
    , fromString
    , toString
    )

import Form.Decoder as Decoder exposing (Decoder, Validator)


type Name
    = Name String


toString : Name -> String
toString (Name str) =
    str


fromString : String -> Name
fromString str =
    Name str


type Error
    = Required


{-| Display error on input fields.
-}
errorField : Error -> String
errorField err =
    case err of
        Required ->
            "シナリオ名を入力してください"


decoder : Decoder String Error Name
decoder =
    Decoder.identity
        |> Decoder.assert notEmpty
        |> Decoder.map Name


notEmpty : Validator String Error
notEmpty =
    Decoder.minLength Required 1
