module Models.Script exposing (..)

import Component.Form as Form
import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onChange)
import Html.Keyed as Keyed
import Html.Lazy as Html
import Json.Decode as D exposing (Value)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Json.Encode.Extra as ExEncode
import List.Extra as ExList
import Models.Board as Board
import Models.Character as Character
import Models.Script.Id as Id exposing (Id)
import Models.Script.Name as Name exposing (Name)
import Models.TragedySet as TragedySet exposing (TragedySet)



-- Core


type alias Script =
    { id : Id
    , name : Name
    , set : TragedySet
    , mainPlot : TragedySet.Plot
    , subPlot1 : TragedySet.Plot
    , subPlot2 : Maybe TragedySet.Plot
    , characters : List Character.CharacterScriptData
    , numberOfLoops : Int
    , daysInOneLoop : Int
    }


type alias IncidentScriptData =
    { incident : TragedySet.Incident
    , day : Int
    , culprit : Character.Character
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
        |> Decoder.field decoderSubPlot1
        |> Decoder.field decoderSubPlot2
        |> Decoder.field decoderCharacters
        |> Decoder.field (Decoder.identity |> Decoder.lift .numberOfLoops)
        |> Decoder.field (Decoder.identity |> Decoder.lift .daysInOneLoop)
        |> Decoder.assert charactersValidator
        |> Decoder.assert characterRolesValidator



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


decoderSubPlot1 : Decoder RegisterForm Error TragedySet.Plot
decoderSubPlot1 =
    Decoder.identity
        |> Decoder.lift .subPlot1


decoderSubPlot2 : Decoder RegisterForm Error (Maybe TragedySet.Plot)
decoderSubPlot2 =
    Decoder.identity
        |> Decoder.lift .subPlot2


decoderCharacters : Decoder RegisterForm Error (List Character.CharacterScriptData)
decoderCharacters =
    Decoder.identity
        |> Decoder.lift .characters



-- Method バリデーション


charactersValidator : Decoder.Validator Script Error
charactersValidator =
    Decoder.custom <|
        \script ->
            if List.length script.characters > 0 then
                Ok ()

            else
                Err [ NoCharacterError ]


characterRolesValidator : Decoder.Validator Script Error
characterRolesValidator =
    Decoder.custom <|
        \script ->
            if List.length (unassignedRoles script) == 0 then
                Ok ()

            else
                Err [ InvalidCharacterRoles ]



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
        |> Pipeline.optional "subPlot1" TragedySet.decoderPlot TragedySet.circleOfFriends
        |> Pipeline.optional "subPlot2" TragedySet.decoderMaybePlot Nothing
        |> Pipeline.optional "characters" (D.list Character.decodeCharacterScriptData) []
        |> Pipeline.optional "numberOfLoops" D.int 1
        |> Pipeline.optional "daysInOneLoop" D.int 1



-- Form


type alias RegisterForm =
    { id : String
    , name : String
    , set : TragedySet
    , mainPlot : TragedySet.Plot
    , subPlot1 : TragedySet.Plot
    , subPlot2 : Maybe TragedySet.Plot
    , characters : List Character.CharacterScriptData
    , numberOfLoops : Int
    , daysInOneLoop : Int
    }


initForm : RegisterForm
initForm =
    { id = ""
    , name = ""
    , set = TragedySet.initBasicTragedy
    , mainPlot = TragedySet.murderPlan
    , subPlot1 = TragedySet.circleOfFriends
    , subPlot2 = Just TragedySet.theHiddenFreak
    , characters = []
    , numberOfLoops = 4
    , daysInOneLoop = 5
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



-- Method Get


getMainPlots : RegisterForm -> List TragedySet.Plot
getMainPlots f =
    TragedySet.filterMainPlots f.set.plots


getSubPlots : RegisterForm -> Maybe TragedySet.Plot -> List TragedySet.Plot
getSubPlots f m =
    case m of
        Nothing ->
            TragedySet.filterSubPlots f.set.plots

        Just plot ->
            TragedySet.filterSubPlots f.set.plots |> List.filter (\p -> plot /= p)


containCharacter : Character.Character -> RegisterForm -> Bool
containCharacter c f =
    List.member c (Character.charactersFromCharacterScriptDataList f.characters)


getScriptRoles : { a | mainPlot : TragedySet.Plot, subPlot1 : TragedySet.Plot, subPlot2 : Maybe TragedySet.Plot } -> List TragedySet.Role
getScriptRoles f =
    TragedySet.filterRoleLimit <|
        case f.subPlot2 of
            Just p ->
                List.concat [ f.mainPlot.roles, f.subPlot1.roles, p.roles ]

            Nothing ->
                List.concat [ f.mainPlot.roles, f.subPlot1.roles ]


unassignedRoles : { a | mainPlot : TragedySet.Plot, subPlot1 : TragedySet.Plot, subPlot2 : Maybe TragedySet.Plot, characters : List Character.CharacterScriptData } -> List TragedySet.Role
unassignedRoles scriptForm =
    let
        characterRoleList =
            Character.rolesFromCharacterScriptDataList scriptForm.characters

        scriptRoleList =
            getScriptRoles scriptForm
    in
    TragedySet.exceptRoleList characterRoleList scriptRoleList



-- Convert


convert : RegisterForm -> Maybe Script
convert f =
    case Decoder.run form f of
        Ok result ->
            Just result

        Err _ ->
            Nothing



-- Method Set


setName : String -> RegisterForm -> RegisterForm
setName s f =
    { f | name = s }


setTragedySet : String -> RegisterForm -> RegisterForm
setTragedySet s f =
    let
        set =
            TragedySet.getTragedySetFromString s
    in
    case set.setType of
        TragedySet.BasicTragedy ->
            { f | set = set, mainPlot = TragedySet.murderPlan, subPlot1 = TragedySet.circleOfFriends, subPlot2 = Just TragedySet.theHiddenFreak }

        TragedySet.FirstSteps ->
            { f | set = set, mainPlot = TragedySet.murderPlan, subPlot1 = TragedySet.circleOfFriends, subPlot2 = Nothing }


setMainPlot : String -> RegisterForm -> RegisterForm
setMainPlot s f =
    { f | mainPlot = TragedySet.plotFromStringWithDefault s }


setSubPlot1 : String -> RegisterForm -> RegisterForm
setSubPlot1 s f =
    { f | subPlot1 = TragedySet.plotFromStringWithDefault s }


setSubPlot2 : String -> RegisterForm -> RegisterForm
setSubPlot2 s f =
    { f | subPlot2 = TragedySet.plotFromString s }


setNumberOfLoops : String -> RegisterForm -> RegisterForm
setNumberOfLoops s f =
    { f | numberOfLoops = Maybe.withDefault 1 <| String.toInt s }


setDaysInOneLoop : String -> RegisterForm -> RegisterForm
setDaysInOneLoop s f =
    { f | daysInOneLoop = Maybe.withDefault 1 <| String.toInt s }


setId : String -> RegisterForm -> RegisterForm
setId s f =
    { f | id = s }


setCharacter : Character.Character -> RegisterForm -> RegisterForm
setCharacter c f =
    { f | characters = Character.characterScriptDataFromCharacter c :: f.characters }


deleteCharacter : Character.Character -> RegisterForm -> RegisterForm
deleteCharacter c f =
    { f | characters = List.filter (\data -> data.character /= c) f.characters }


setCharacterRole : Character.CharacterScriptData -> String -> RegisterForm -> RegisterForm
setCharacterRole c s f =
    let
        characters =
            List.map
                (\data ->
                    if data == c then
                        { data | role = TragedySet.roleFromString s }

                    else
                        data
                )
                f.characters
    in
    { f | characters = characters }


setCharacterOptionalNumber : Character.CharacterScriptData -> String -> RegisterForm -> RegisterForm
setCharacterOptionalNumber c s f =
    let
        characters =
            List.map
                (\data ->
                    if data == c then
                        { data | optionalNumber = String.toInt s }

                    else
                        data
                )
                f.characters
    in
    { f | characters = characters }


setCharacterTurf : Character.CharacterScriptData -> String -> RegisterForm -> RegisterForm
setCharacterTurf c s f =
    let
        characters =
            List.map
                (\data ->
                    if data == c then
                        { data | turf = Board.boardFromString s }

                    else
                        data
                )
                f.characters
    in
    { f | characters = characters }


registerForm : String -> List (Html msg) -> Html msg
registerForm title children =
    div []
        [ h2 [] [ text title ]
        , div []
            children
        ]



-- Error


type Error
    = NameError Name.Error
    | IdError Id.Error
    | TragedySetError TragedySet.Error
    | NoCharacterError
    | InvalidCharacterRoles


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



-- View


mainPlots : (String -> msg) -> TragedySet.Plot -> RegisterForm -> Html msg
mainPlots chgMsg selectedPlot scriptForm =
    let
        plotList =
            getMainPlots scriptForm

        plotKey =
            TragedySet.plotToString selectedPlot

        optionList =
            List.map (\p -> Tuple.pair (TragedySet.plotToString p) p.name) plotList
    in
    Form.select "-main-plot" chgMsg plotKey optionList


subPlots1 : (String -> msg) -> TragedySet.Plot -> RegisterForm -> Html msg
subPlots1 chgMsg selectedPlot scriptForm =
    let
        plotList =
            getSubPlots scriptForm scriptForm.subPlot2

        plotKey =
            TragedySet.plotToString selectedPlot

        optionList =
            List.map (\p -> Tuple.pair (TragedySet.plotToString p) p.name) plotList
    in
    Form.select "-sub-plot-1" chgMsg plotKey optionList


subPlots2 : (String -> msg) -> Maybe TragedySet.Plot -> RegisterForm -> Html msg
subPlots2 chgMsg maybeSelectedPlot scriptForm =
    case maybeSelectedPlot of
        Nothing ->
            text "エラー。ありえないパターンです。管理者に現在のURLを伝えてください。"

        Just selectedPlot ->
            let
                plotList =
                    getSubPlots scriptForm (Just scriptForm.subPlot1)

                plotKey =
                    TragedySet.plotToString selectedPlot

                optionList =
                    List.map (\p -> Tuple.pair (TragedySet.plotToString p) p.name) plotList
            in
            Form.select "-sub-plot-2" chgMsg plotKey optionList


characterRoles : Character.CharacterScriptData -> (String -> msg) -> RegisterForm -> Html msg
characterRoles char chgMsg scriptForm =
    let
        exceptList =
            unassignedRoles scriptForm

        roleList =
            case char.role of
                Just role ->
                    role :: TragedySet.person :: exceptList

                Nothing ->
                    TragedySet.person :: exceptList

        roleKey =
            TragedySet.roleToString (Maybe.withDefault TragedySet.person char.role)

        optionList =
            List.map (\r -> Tuple.pair (TragedySet.roleToString r) r.name) roleList
    in
    Form.select ("-character-roles-" ++ Character.characterToString char.character) chgMsg roleKey optionList



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
        , ( "subPlot1", E.string <| TragedySet.plotToString script.subPlot1 )
        , ( "subPlot2", ExEncode.maybe E.string <| Maybe.map TragedySet.plotToString script.subPlot2 )
        , ( "characters", E.list Character.encodeCharacterScriptData script.characters )
        , ( "numberOfLoops", E.int script.numberOfLoops )
        , ( "daysInOneLoop", E.int script.daysInOneLoop )
        ]
