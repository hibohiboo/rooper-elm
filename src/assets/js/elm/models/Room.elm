module Models.Room exposing (..)

import Component.Form as Form
import Component.Link
import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, href, style)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Models.Room.Id as Id exposing (Id)
import Models.Room.Name as Name exposing (Name)
import Models.Script as Script exposing (Script)
import Models.ScriptName as ScriptName exposing (ScriptName)



-- Core


type alias Room =
    { id : Id
    , name : Name
    , script : Script
    , mastermindTwitterScreenName : String
    , protagonist1TwitterScreenName : String
    , protagonist2TwitterScreenName : String
    , protagonist3TwitterScreenName : String
    , isUseTweet : Bool
    , isUseTweetRoomName : Bool
    }


initRoom : Maybe Room
initRoom =
    Nothing


getId : Room -> String
getId room =
    Id.toString room.id



-- Decoder


form : Decoder RegisterForm Error Room
form =
    Decoder.top Room
        |> Decoder.field decoderId
        |> Decoder.field decoderName
        |> Decoder.field decoderScript
        |> Decoder.field (Decoder.identity |> Decoder.assert (Decoder.minLength RequiredMastermindTwitterScreenName 1) |> Decoder.lift .mastermindTwitterScreenName)
        |> Decoder.field (Decoder.identity |> Decoder.assert (Decoder.minLength RequiredProtagonist1TwitterScreenName 1) |> Decoder.lift .protagonist1TwitterScreenName)
        |> Decoder.field (Decoder.identity |> Decoder.assert (Decoder.minLength RequiredProtagonist2TwitterScreenName 1) |> Decoder.lift .protagonist2TwitterScreenName)
        |> Decoder.field (Decoder.identity |> Decoder.assert (Decoder.minLength RequiredProtagonist3TwitterScreenName 1) |> Decoder.lift .protagonist3TwitterScreenName)
        |> Decoder.field (Decoder.identity |> Decoder.lift .isUseTweet)
        |> Decoder.field (Decoder.identity |> Decoder.lift .isUseTweetRoomName)


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


decoderScript : Decoder RegisterForm Error Script
decoderScript =
    customScript
        |> Decoder.lift .script


customScript : Decoder (Maybe Script) Error Script
customScript =
    Decoder.custom <| Result.fromMaybe [ RequiredScript ]



-- Form


type alias RegisterForm =
    { id : String
    , name : String
    , scriptId : String
    , mastermindTwitterScreenName : String
    , protagonist1TwitterScreenName : String
    , protagonist2TwitterScreenName : String
    , protagonist3TwitterScreenName : String
    , script : Maybe Script
    , isUseTweet : Bool
    , isUseTweetRoomName : Bool
    }


init : RegisterForm
init =
    { id = ""
    , name = ""
    , scriptId = ""
    , mastermindTwitterScreenName = ""
    , protagonist1TwitterScreenName = ""
    , protagonist2TwitterScreenName = ""
    , protagonist3TwitterScreenName = ""
    , script = Nothing
    , isUseTweet = True
    , isUseTweetRoomName = True
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
    | RequiredScript
    | RequiredMastermindTwitterScreenName
    | RequiredProtagonist1TwitterScreenName
    | RequiredProtagonist2TwitterScreenName
    | RequiredProtagonist3TwitterScreenName


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


setScript : Maybe Script -> RegisterForm -> RegisterForm
setScript s f =
    { f | script = s }


setMastermindTwitterScreenName : String -> RegisterForm -> RegisterForm
setMastermindTwitterScreenName s f =
    { f | mastermindTwitterScreenName = s }


setProtagonist1TwitterScreenName : String -> RegisterForm -> RegisterForm
setProtagonist1TwitterScreenName s f =
    { f | protagonist1TwitterScreenName = s }


setProtagonist2TwitterScreenName : String -> RegisterForm -> RegisterForm
setProtagonist2TwitterScreenName s f =
    { f | protagonist2TwitterScreenName = s }


setProtagonist3TwitterScreenName : String -> RegisterForm -> RegisterForm
setProtagonist3TwitterScreenName s f =
    { f | protagonist3TwitterScreenName = s }


setIsUseTweet : Bool -> RegisterForm -> RegisterForm
setIsUseTweet s f =
    { f | isUseTweet = s }


setIsUseTweetRoomName : Bool -> RegisterForm -> RegisterForm
setIsUseTweetRoomName s f =
    { f | isUseTweetRoomName = s }



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
        |> Pipeline.optional "script" (D.at [ "id" ] D.string) ""
        |> Pipeline.optional "mastermindTwitterScreenName" D.string ""
        |> Pipeline.optional "protagonist1TwitterScreenName" D.string ""
        |> Pipeline.optional "protagonist2TwitterScreenName" D.string ""
        |> Pipeline.optional "protagonist3TwitterScreenName" D.string ""
        |> Pipeline.optional "script" Script.scriptDecoder Nothing
        |> Pipeline.optional "isUseTweet" D.bool False
        |> Pipeline.optional "isUseTweetRoomName" D.bool False



-- View
-- Atomic view only for this register form


registerForm : List (Html msg) -> Html msg
registerForm children =
    div []
        [ h2 [] [ text "ルーム編集" ]
        , Component.Link.backPrevPageRight "/rooper/"
        , div []
            children
        ]



-- Views


scriptSelectForm : (String -> msg) -> List ScriptName -> RegisterForm -> Html msg
scriptSelectForm chgMsg list f =
    let
        key =
            f.scriptId

        optionList =
            ( "未選択", "未選択" ) :: List.map (\item -> Tuple.pair item.id item.name) list
    in
    Form.select "-room-script" chgMsg key optionList



-- Decoder


decodeRoomFromJson : D.Value -> Maybe Room
decodeRoomFromJson json =
    json
        |> D.decodeValue (D.map convert formDecoder)
        |> Result.toMaybe
        |> Maybe.withDefault Nothing



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : Room -> E.Value
encode room =
    E.object
        [ ( "id", E.string <| Id.toString room.id )
        , ( "name", E.string <| Name.toString room.name )
        , ( "script", Script.encode room.script )
        , ( "mastermindTwitterScreenName", E.string room.mastermindTwitterScreenName )
        , ( "protagonist1TwitterScreenName", E.string room.protagonist1TwitterScreenName )
        , ( "protagonist2TwitterScreenName", E.string room.protagonist2TwitterScreenName )
        , ( "protagonist3TwitterScreenName", E.string room.protagonist3TwitterScreenName )
        , ( "isUseTweet", E.bool room.isUseTweet )
        , ( "isUseTweetRoomName", E.bool room.isUseTweetRoomName )
        ]
