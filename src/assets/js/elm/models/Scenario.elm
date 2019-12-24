module Models.Scenario exposing (..)

import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Json.Decode as D exposing (Value)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E
import Models.Scenario.Id as Id exposing (Id)
import Models.Scenario.Name as Name exposing (Name)
import Models.TragedySet as TragedySet exposing (TragedySet)



-- Core


type alias Scenario =
    { id : Id
    , name : Name
    , set : TragedySet
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
        |> Decoder.field decoderTragedySet


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


decoderTragedySet : Decoder RegisterForm Error TragedySet
decoderTragedySet =
    TragedySet.decoder
        |> Decoder.mapError TragedySetError
        |> Decoder.lift .set



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
        |> hardcoded "Basic Tragedy X"



-- Form


type alias RegisterForm =
    { id : String
    , name : String
    , set : String
    }


initForm : RegisterForm
initForm =
    { id = ""
    , name = ""
    , set = "Basic Tragedy X"
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
    | TragedySetError TragedySet.Error


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


registerForm : String -> List (Html msg) -> Html msg
registerForm title children =
    div []
        [ h2 [] [ text title ]
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
        , ( "set", E.string <| TragedySet.toString scenario.set )
        ]
