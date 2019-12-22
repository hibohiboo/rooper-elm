module Component.Bulma exposing (..)

import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (onClick)


modal : Bool -> msg -> Html msg -> Html msg
modal isActive closeMessage content =
    let
        isActiveClass =
            if isActive then
                "is-active"

            else
                ""
    in
    div [ class "modal", class isActiveClass ]
        [ div [ class "modal-background", onClick closeMessage ] []
        , div [ class "modal-content" ]
            [ content
            ]
        ]
