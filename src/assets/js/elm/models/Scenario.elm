module Models.Scenario exposing (..)

import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Json.Decode as D exposing (Value)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E
import Models.Scenario.Id as Id exposing (Id)
import Models.Scenario.Name as Name exposing (Name)



-- Core


type alias Scenario =
    { id : Id
    , name : Name
    }


initScenario : Maybe Scenario
initScenario =
    Nothing



-- Decoder


form : Decoder RegisterForm Error Scenario
form =
    Decoder.top Scenario
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



-- Decoder Register Form


decodeScenarioRegisterFormFromJson : Value -> Maybe RegisterForm
decodeScenarioRegisterFormFromJson json =
    json
        |> D.decodeValue formDecoder
        |> Result.toMaybe


formDecoder : D.Decoder RegisterForm
formDecoder =
    D.succeed RegisterForm
        |> required "id" D.string
        |> required "name" D.string



-- Form


type alias RegisterForm =
    { id : String
    , name : String
    }


initForm : RegisterForm
initForm =
    { id = ""
    , name = ""
    }



-- Convert


convert : RegisterForm -> Maybe Scenario
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


registerForm : List (Html msg) -> Html msg
registerForm children =
    div []
        [ h2 [] [ text "シナリオ作成" ]
        , div []
            children
        ]



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : Scenario -> E.Value
encode scenario =
    E.object
        [ ( "id", E.string <| Id.toString scenario.id )
        , ( "name", E.string <| Name.toString scenario.name )
        ]
