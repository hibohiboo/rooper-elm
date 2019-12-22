module Models.Scenario.TragedySet exposing
    ( Error(..)
    , TragedySet
    , decoder
    , errorField
    , toString
    )

import Form.Decoder as Decoder exposing (Decoder)


type TragedySet
    = TragedySet String


toString : TragedySet -> String
toString (TragedySet str) =
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


decoder : Decoder String Error TragedySet
decoder =
    Decoder.identity
        |> Decoder.map TragedySet
