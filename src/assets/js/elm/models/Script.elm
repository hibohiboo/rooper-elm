module Models.Script exposing (..)

import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onChange)
import Html.Keyed as Keyed
import Html.Lazy as Html
import Json.Decode as D exposing (Value)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.Script.Id as Id exposing (Id)
import Models.Script.Name as Name exposing (Name)
import Models.TragedySet as TragedySet exposing (TragedySet)



-- Core


type alias Script =
    { id : Id
    , name : Name
    , set : TragedySet
    , mainPlot : TragedySet.Plot
    }


initScript : Maybe Script
initScript =
    Nothing



-- Decoder


form : Decoder RegisterForm Error Script
form =
    Decoder.top Script
        |> Decoder.field decoderId
        |> Decoder.field decoderName
        |> Decoder.field decoderTragedySet
        |> Decoder.field decoderMainPlot



-- ルールYはそのまま入れる


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


decoderMainPlot : Decoder RegisterForm Error TragedySet.Plot
decoderMainPlot =
    Decoder.identity
        |> Decoder.lift .mainPlot



-- Decoder Register Form


decodeScriptRegisterFormFromJson : Value -> Maybe RegisterForm
decodeScriptRegisterFormFromJson json =
    json
        |> D.decodeValue formDecoder
        |> Result.toMaybe


formDecoder : D.Decoder RegisterForm
formDecoder =
    D.succeed RegisterForm
        |> Pipeline.required "id" D.string
        |> Pipeline.required "name" D.string
        |> Pipeline.optional "set" TragedySet.decoderTragedySet TragedySet.initBasicTragedy
        |> Pipeline.optional "mainPlot" TragedySet.decoderPlot TragedySet.murderPlan



-- Form


type alias RegisterForm =
    { id : String
    , name : String
    , set : TragedySet
    , mainPlot : TragedySet.Plot
    }


initForm : RegisterForm
initForm =
    { id = ""
    , name = ""
    , set = TragedySet.initBasicTragedy
    , mainPlot = TragedySet.murderPlan
    }


isSetBasicTragedy : RegisterForm -> Bool
isSetBasicTragedy f =
    case f.set.setType of
        TragedySet.BasicTragedy ->
            True

        _ ->
            False


isSetFirstSteps : RegisterForm -> Bool
isSetFirstSteps f =
    case f.set.setType of
        TragedySet.FirstSteps ->
            True

        _ ->
            False


getMainPlots : RegisterForm -> List TragedySet.Plot
getMainPlots f =
    TragedySet.getMainPlot f.set.plots



-- Convert


convert : RegisterForm -> Maybe Script
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


setTragedySet : String -> RegisterForm -> RegisterForm
setTragedySet s f =
    { f | set = TragedySet.getTragedySetFromString s }


setMainPlot : String -> RegisterForm -> RegisterForm
setMainPlot s f =
    { f | mainPlot = TragedySet.plotFromStringWithDefault s }


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


mainPlots : (String -> msg) -> List TragedySet.Plot -> Html msg
mainPlots chgMsg rs =
    Keyed.node "select"
        [ onChange chgMsg ]
    <|
        List.map keyedPlot rs


keyedPlot : TragedySet.Plot -> ( String, Html msg )
keyedPlot p =
    ( p.name ++ "-main-plot", Html.lazy plot p )


plot : TragedySet.Plot -> Html msg
plot p =
    option
        [ value (TragedySet.plotToString p) ]
        [ text p.name
        ]



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : Script -> E.Value
encode script =
    E.object
        [ ( "id", E.string <| Id.toString script.id )
        , ( "name", E.string <| Name.toString script.name )
        , ( "set", E.string <| TragedySet.toString script.set )
        , ( "mainPlot", E.string <| TragedySet.plotToString script.mainPlot )
        ]
