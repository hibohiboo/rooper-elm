module Views.Swiper exposing (wrapper)

import Html exposing (..)
import Html.Attributes exposing (..)


wrapper : List (Html msg) -> Html msg
wrapper contents =
    div [ class "swiper-container my-sw-container" ]
        [ div [ class "swiper-wrapper" ]
            contents
        ]
