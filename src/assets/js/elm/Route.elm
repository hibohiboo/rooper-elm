module Route exposing (Route(..), toRoute)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, fragment, int, map, oneOf, parse, s, string, top)


type Route
    = Top
    | Script
    | ScriptCreate
    | ScriptEdit String
    | RoomEdit String
    | NotFound


parse : Url -> Maybe Route
parse url =
    Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Top top
        , map Top (s "rooper")
        , map Script (s "rooper" </> s "script")
        , map ScriptCreate (s "rooper" </> s "script" </> s "create")
        , map ScriptEdit (s "rooper" </> s "script" </> s "edit" </> string)
        , map RoomEdit (s "rooper" </> s "room" </> s "edit" </> string)
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
