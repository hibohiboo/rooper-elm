module Models.Room exposing (..)

import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Json.Encode as E
import Models.Room.Id as Id exposing (Id)
import Models.Room.Name as Name exposing (Name)



-- Core


type alias Room =
    { id : Id
    , name : Name
    }


initRoom : Maybe Room
initRoom =
    Nothing



-- Decoder


form : Decoder RegisterForm Error Room
form =
    Decoder.top Room
        |> Decoder.field decoderId
        |> Decoder.field decoderName


decoderName : Decoder RegisterForm Error Name
decoderName =
    Name.decoder
        |> Decoder.mapError NameError
        |> Decoder.lift .name


decoderId : Decoder RegisterForm Error Id
decoderId =
    Id.decoder
        |> Decoder.mapError IdError
        |> Decoder.lift .id



-- Form


type alias RegisterForm =
    { id : String
    , name : String
    }


init : RegisterForm
init =
    { id = ""
    , name = ""
    }



-- Convert


convert : RegisterForm -> Maybe Room
convert f =
    case Decoder.run form f of
        Ok result ->
            Just result

        Err _ ->
            Nothing



-- Error


type Error
    = NameError Name.Error
    | IdError Id.Error


errors : RegisterForm -> List Error
errors f =
    Decoder.errors form f


getNameError : RegisterForm -> List ( String, Bool )
getNameError f =
    List.map
        (\err ->
            case err of
                NameError e ->
                    ( Name.errorField e, True )

                _ ->
                    ( "", False )
        )
        (errors f)


setName : String -> RegisterForm -> RegisterForm
setName s f =
    { f | name = s }


setId : String -> RegisterForm -> RegisterForm
setId s f =
    { f | id = s }



-- Atomic view only for listing registered goats
-- pageTitle : String -> Html msg
-- pageTitle t =
--     Html.h1 [] [ text t ]
-- rooms : List Room -> Html msg
-- rooms rs =
--     div []
--         [ pageTitle "List of rooms"
--         , Keyed.node "div"
--             []
--           <|
--             List.map keyedRoom rs
--         ]
-- keyedRoom : Room -> ( String, Html msg )
-- keyedRoom r =
--     ( Name.toString r.name, Html.lazy room r )
-- room : Room -> Html msg
-- room r =
--     div
--         [ class "roomWrapper"
--         ]
--         [ div
--             [ class "room"
--             ]
--             [ roomField "Name" <| Name.toString r.name
--             , roomField "Id" <| Id.toString r.id
--             ]
--         ]
-- roomField : String -> String -> Html msg
-- roomField title content =
--     div
--         [ class "roomField"
--         ]
--         [ div
--             [ class "roomTitle"
--             ]
--             [ text title
--             ]
--         , div
--             [ class "roomContent"
--             ]
--             [ text content
--             ]
--         ]
-- Atomic view only for this register form


registerForm : List (Html msg) -> Html msg
registerForm children =
    div []
        [ h2 [] [ text "ルーム作成" ]
        , div []
            children
        ]



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : Room -> E.Value
encode room =
    E.object
        [ ( "id", E.string <| Id.toString room.id )
        , ( "name", E.string <| Name.toString room.name )
        ]
