module Component.Form exposing (..)

import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (onClick)



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


createButton : msg -> Html msg
createButton message =
    button [ class "button is-primary", onClick message ]
        [ span [ class "icon" ]
            [ i [ class "fas fa-plus" ] []
            ]
        , span [] [ text "create" ]
        ]
