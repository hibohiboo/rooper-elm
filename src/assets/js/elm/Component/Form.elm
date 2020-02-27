module Component.Form exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import Html.Keyed as Keyed
import Html.Lazy as Html



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


createButton : msg -> String -> Html msg
createButton message buttonText =
    button [ class "button is-primary", onClick message ]
        [ span [ class "icon" ]
            [ i [ class "fas fa-plus" ] []
            ]
        , span [] [ text buttonText ]
        ]


select : String -> (String -> msg) -> String -> List ( String, String ) -> Html msg
select nodeKeySuffix chgMsg selectedKey rs =
    Keyed.node "select" [ onChange chgMsg ] <| List.indexedMap (\i r -> keyedOption i nodeKeySuffix r selectedKey) rs


keyedOption : Int -> String -> ( String, String ) -> String -> ( String, Html msg )
keyedOption i nodeKeySuffix tuple selectedKey =
    let
        key =
            Tuple.first tuple
    in
    ( key ++ String.fromInt i ++ nodeKeySuffix, Html.lazy (selectOption tuple) (key == selectedKey) )


selectOption : ( String, String ) -> Bool -> Html msg
selectOption ( key, txt ) isSelected =
    option
        [ value key, selected isSelected ]
        [ text txt
        ]
