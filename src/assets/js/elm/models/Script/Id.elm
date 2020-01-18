module Models.Script.Id exposing
    ( Error(..)
    , Id
    , decoder
    , errorField
    , fromString
    , toString
    )

import Form.Decoder as Decoder exposing (Decoder)


type Id
    = Id String


toString : Id -> String
toString (Id str) =
    str


fromString : String -> Id
fromString str =
    Id str


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
