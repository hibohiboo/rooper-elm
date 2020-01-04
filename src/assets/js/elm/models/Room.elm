module Models.Room exposing (..)

import Component.Form as Form
import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.Room.Id as Id exposing (Id)
import Models.Room.Name as Name exposing (Name)
import Models.ScriptName as ScriptName exposing (ScriptName)



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
    , scriptId : String
    }


init : RegisterForm
init =
    { id = ""
    , name = ""
    , scriptId = ""
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


setScriptId : String -> RegisterForm -> RegisterForm
setScriptId s f =
    { f | scriptId = s }



-- Decoder Register Form


decodeRegisterFormFromJson : D.Value -> Maybe RegisterForm
decodeRegisterFormFromJson json =
    json
        |> D.decodeValue formDecoder
        |> Result.toMaybe


formDecoder : D.Decoder RegisterForm
formDecoder =
    D.succeed RegisterForm
        |> Pipeline.required "id" D.string
        |> Pipeline.required "name" D.string
        |> Pipeline.optional "scriptId" D.string ""



-- View
-- Atomic view only for this register form


registerForm : List (Html msg) -> Html msg
registerForm children =
    div []
        [ h2 [] [ text "ルーム編集" ]
        , div []
            children
        ]



-- Views


script : (String -> msg) -> List ScriptName -> RegisterForm -> Html msg
script chgMsg list f =
    let
        key =
            f.scriptId

        optionList =
            ( "", "脚本を選択してください" ) :: List.map (\item -> Tuple.pair item.id item.name) list
    in
    Form.select "-main-plot" chgMsg key optionList



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : Room -> E.Value
encode room =
    E.object
        [ ( "id", E.string <| Id.toString room.id )
        , ( "name", E.string <| Name.toString room.name )
        ]
