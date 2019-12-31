module Models.Board exposing (..)

import Json.Decode as D


type alias Board =
    { boardType : BoardType
    , name : String
    }


type BoardType
    = Shrine
    | School
    | Hospital
    | City


decodeBoard : D.Decoder (Maybe Board)
decodeBoard =
    D.map boardFromString D.string


boardToString : Board -> String
boardToString board =
    case board.boardType of
        Shrine ->
            "Shrine"

        School ->
            "School"

        Hospital ->
            "Hospital"

        City ->
            "City"


boardFromString : String -> Maybe Board
boardFromString s =
    case s of
        "Shrine" ->
            Just shrine

        "School" ->
            Just school

        "Hospital" ->
            Just hospital

        "City" ->
            Just city

        _ ->
            Nothing


shrine : Board
shrine =
    Board Shrine "神社"


school : Board
school =
    Board School "学校"


hospital : Board
hospital =
    Board Hospital "病院"


city : Board
city =
    Board City "都市"


boards : List Board
boards =
    [ shrine
    , school
    , hospital
    , city
    ]
