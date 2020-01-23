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


decode : D.Decoder Board
decode =
    D.map (\b -> Maybe.withDefault shrine b) decodeBoard


decodeBoard : D.Decoder (Maybe Board)
decodeBoard =
    D.map boardFromString D.string


toString : Board -> String
toString board =
    boardTypeToString board.boardType


boardTypeToString : BoardType -> String
boardTypeToString t =
    case t of
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


boardTypeToName : BoardType -> String
boardTypeToName t =
    boardTypeToBoard t |> .name


boardTypeToBoard : BoardType -> Board
boardTypeToBoard t =
    case t of
        Shrine ->
            shrine

        School ->
            school

        Hospital ->
            hospital

        City ->
            city


boardTypeFromString : String -> Maybe BoardType
boardTypeFromString s =
    boardFromString s |> Maybe.map .boardType


boardToCardUrl : BoardType -> String
boardToCardUrl t =
    "/assets/images/boards/cards/"
        ++ (case t of
                City ->
                    "city.png"

                Shrine ->
                    "shrine.png"

                School ->
                    "school.png"

                Hospital ->
                    "hospital.png"
           )


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
