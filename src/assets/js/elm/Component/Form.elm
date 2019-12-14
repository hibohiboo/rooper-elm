module Component.Form exposing (..)

import Html exposing (..)
import Html.Attributes as Attributes exposing (..)



-- https://bulma.io/documentation/form/general/


field : List (Html msg) -> Html msg
field children =
    div [ class "field" ] children


control : List (Html msg) -> Html msg
control children =
    div [ class "control" ] children


errors : List ( String, Bool ) -> Html msg
errors messages =
    p [ class "help" ] <|
        List.map
            (\( message, invalid ) ->
                if invalid then
                    text message

                else
                    text ""
            )
            messages
