module Component.Link exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


backPrevPageRight : String -> Html msg
backPrevPageRight prevUrl =
    div [ style "text-align" "right" ]
        [ a [ href prevUrl ] [ text "戻る" ]
        ]
