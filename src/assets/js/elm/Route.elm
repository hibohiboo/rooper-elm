module Route exposing (Route(..), toRoute)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, fragment, int, map, oneOf, parse, s, string, top)


type Route
    = Top
    | Scenario
    | NotFound


parse : Url -> Maybe Route
parse url =
    Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Top top
        , map Top (s "rooper")
        , map Scenario (s "rooper" </> s "scenario")
        ]


toRoute : String -> Route
toRoute string =
    -- let
    --     _ =
    --         Debug.log "route" string
    -- in
    case Url.fromString string of
        Nothing ->
            NotFound

        Just url ->
            Maybe.withDefault NotFound (Parser.parse parser url)
