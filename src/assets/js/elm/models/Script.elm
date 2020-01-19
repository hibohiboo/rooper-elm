module Models.Script exposing (..)

import Component.Form as Form
import Component.Link
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
import Models.RoomData.OpenSheet as OpenSheet exposing (OpenSheet)
import Models.Script.Id as Id exposing (Id)
import Models.Script.IncidentScriptData as IncidentScriptData exposing (IncidentScriptData)
import Models.Script.Name as Name exposing (Name)
import Models.TragedySet as TragedySet exposing (TragedySet)
import Models.Utility.List as UtilityList



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
    , incidents : List IncidentScriptData
    , extra : String
    , memo : String
    }


initScript : Maybe Script
initScript =
    Nothing


initDefault : Script
initDefault =
    { id = Id.fromString "forTestScript"
    , name = Name.fromString "テスト用脚本"
    , set = TragedySet.initBasicTragedy
    , mainPlot = TragedySet.murderPlan
    , subPlot1 = TragedySet.circleOfFriends
    , subPlot2 = Just TragedySet.theHiddenFreak
    , characters = [ Character.characterToScriptData Character.shrineMaiden, Character.characterToScriptData Character.illusion ]
    , numberOfLoops = 4
    , daysInOneLoop = 5
    , incidents = []
    , extra = "相談不可。"
    , memo = "【シナリオの特徴】"
    }


getId : Script -> String
getId s =
    Id.toString s.id



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
        |> Decoder.field (Decoder.identity |> Decoder.lift .incidents)
        |> Decoder.field (Decoder.identity |> Decoder.lift .extra)
        |> Decoder.field (Decoder.identity |> Decoder.lift .memo)
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



-- Decoder Script


scriptDecoder : D.Decoder (Maybe Script)
scriptDecoder =
    D.map convert formDecoder


decodeScriptFromJson : Value -> Maybe Script
decodeScriptFromJson json =
    json
        |> D.decodeValue scriptDecoder
        |> Result.toMaybe
        |> Maybe.withDefault Nothing



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
        |> Pipeline.optional "incidentDay" D.int 1
        |> Pipeline.optional "incident" D.string ""
        |> Pipeline.optional "incidentCulprit" D.string ""
        |> Pipeline.optional "incidents" (D.list IncidentScriptData.decode) []
        |> Pipeline.optional "extra" D.string ""
        |> Pipeline.optional "memo" D.string ""



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
    , incidentDay : Int
    , incident : String
    , incidentCulprit : String
    , incidents : List IncidentScriptData
    , extra : String
    , memo : String
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
    , incidentDay = 1
    , incident = ""
    , incidentCulprit = ""
    , incidents = []
    , extra = "相談不可。 \n最後の戦いあり。"
    , memo = "【シナリオの特徴】\nここに書かれた内容は、主人公には公開されません。\n【脚本家への指針】"
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
    UtilityList.exceptList characterRoleList scriptRoleList


mysteryBoyRoles : RegisterForm -> List TragedySet.Role
mysteryBoyRoles scriptForm =
    let
        list =
            unassignedRoles scriptForm
    in
    List.filter (\r -> not <| List.member r list) scriptForm.set.roles


unassignedIncidentDays : RegisterForm -> List Int
unassignedIncidentDays f =
    let
        daysList =
            List.range 1 f.daysInOneLoop

        assignedList =
            IncidentScriptData.assignedIncidentDays f.incidents
    in
    List.filter (\day -> not <| List.member day assignedList) daysList


unassignedCulpritCharacters : RegisterForm -> List Character.Character
unassignedCulpritCharacters f =
    let
        charactersList =
            Character.charactersFromCharacterScriptDataList f.characters
                |> List.reverse

        assignedList =
            IncidentScriptData.assignedCulpritCharacters f.incidents
    in
    List.filter (\c -> not <| List.member c assignedList) charactersList



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


setExtra : String -> RegisterForm -> RegisterForm
setExtra s f =
    { f | extra = s }


setMemo : String -> RegisterForm -> RegisterForm
setMemo s f =
    { f | memo = s }


setTragedySet : String -> RegisterForm -> RegisterForm
setTragedySet s f =
    let
        set =
            TragedySet.getTragedySetFromString s
    in
    case set.setType of
        TragedySet.BasicTragedy ->
            let
                new =
                    { f | set = set, mainPlot = TragedySet.murderPlan, subPlot1 = TragedySet.circleOfFriends, subPlot2 = Just TragedySet.theHiddenFreak, incidents = [] }
            in
            { new | characters = resetRolesCharacterScriptDataList new }

        TragedySet.FirstSteps ->
            let
                new =
                    { f | set = set, mainPlot = TragedySet.murderPlan, subPlot1 = TragedySet.shadowOfTheRipper, subPlot2 = Nothing, incidents = [] }
            in
            { new | characters = resetRolesCharacterScriptDataList new }
        -- TODO: 実際に作る
        TragedySet.MysteryCircle ->
            let
                new =
                    { f | set = set, mainPlot = TragedySet.murderPlan, subPlot1 = TragedySet.shadowOfTheRipper, subPlot2 = Nothing, incidents = [] }
            in
            { new | characters = resetRolesCharacterScriptDataList new }


resetRolesCharacterScriptDataList : RegisterForm -> List Character.CharacterScriptData
resetRolesCharacterScriptDataList f =
    List.map
        (\c ->
            case c.character.characterType of
                Character.MysteryBoy ->
                    { c | role = List.head <| mysteryBoyRoles f }

                _ ->
                    { c | role = Nothing }
        )
        f.characters


setMainPlot : String -> RegisterForm -> RegisterForm
setMainPlot s f =
    let
        new =
            { f | mainPlot = TragedySet.plotFromStringWithDefault s }
    in
    { new | characters = resetRolesCharacterScriptDataList new }


setSubPlot1 : String -> RegisterForm -> RegisterForm
setSubPlot1 s f =
    let
        new =
            { f | subPlot1 = TragedySet.plotFromStringWithDefault s }
    in
    { new | characters = resetRolesCharacterScriptDataList new }


setSubPlot2 : String -> RegisterForm -> RegisterForm
setSubPlot2 s f =
    let
        new =
            { f | subPlot2 = TragedySet.plotFromString s }
    in
    { new | characters = resetRolesCharacterScriptDataList new }


setNumberOfLoops : String -> RegisterForm -> RegisterForm
setNumberOfLoops s f =
    { f | numberOfLoops = Maybe.withDefault 1 <| String.toInt s }


setDaysInOneLoop : String -> RegisterForm -> RegisterForm
setDaysInOneLoop s f =
    let
        daysInOneLoop =
            Maybe.withDefault 1 <| String.toInt s
    in
    { f | daysInOneLoop = daysInOneLoop, incidents = List.filter (\i -> i.day <= daysInOneLoop) f.incidents }


setId : String -> RegisterForm -> RegisterForm
setId s f =
    { f | id = s }


setIntIncidentDay : Int -> RegisterForm -> RegisterForm
setIntIncidentDay i f =
    { f | incidentDay = i }


setIncidentDay : String -> RegisterForm -> RegisterForm
setIncidentDay s f =
    { f | incidentDay = Maybe.withDefault 1 <| String.toInt s }


setIncident : String -> RegisterForm -> RegisterForm
setIncident s f =
    { f | incident = s }


setIncidentCulprit : String -> RegisterForm -> RegisterForm
setIncidentCulprit s f =
    { f | incidentCulprit = s }


addIncidents : RegisterForm -> RegisterForm
addIncidents f =
    let
        -- TODO: フォーム上からは失敗しない想定でwithDefaultを使用
        incident =
            Maybe.withDefault TragedySet.murder (TragedySet.incidentFromString f.incident)

        char =
            Maybe.withDefault Character.boyStudent (Character.characterFromString f.incidentCulprit)
    in
    { f
        | incidents =
            IncidentScriptData
                incident
                f.incidentDay
                char
                :: f.incidents
    }


deleteIncidents : IncidentScriptData -> RegisterForm -> RegisterForm
deleteIncidents d f =
    { f | incidents = List.filter (\data -> data /= d) f.incidents }


setCharacter : Character.Character -> RegisterForm -> RegisterForm
setCharacter c f =
    { f | characters = Character.characterScriptDataFromCharacter c :: f.characters }


deleteCharacter : Character.Character -> RegisterForm -> RegisterForm
deleteCharacter c f =
    { f | characters = List.filter (\data -> data.character /= c) f.characters, incidents = List.filter (\i -> i.culprit /= c) f.incidents }


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
        , Component.Link.backPrevPageRight "/rooper/script/"
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



-- Open Sheet


scriptToOpenSheet : Script -> OpenSheet
scriptToOpenSheet { set, numberOfLoops, daysInOneLoop, incidents, extra } =
    OpenSheet set numberOfLoops daysInOneLoop (List.map IncidentScriptData.incidentToOpenSheetIncident incidents) extra



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
            if char.character.characterType == Character.MysteryBoy then
                mysteryBoyRoles scriptForm

            else
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


incidentsSelect : (String -> msg) -> RegisterForm -> Html msg
incidentsSelect chgMsg scriptForm =
    let
        roleKey =
            ""

        optionList =
            List.map (\incident -> Tuple.pair (TragedySet.incidentToString incident) incident.name) scriptForm.set.incidents
    in
    Form.select "-incidents" chgMsg roleKey optionList



-- TODO: 選択した日付は選べないように


incidentDays : (String -> msg) -> RegisterForm -> Html msg
incidentDays chgMsg scriptForm =
    let
        roleKey =
            String.fromInt scriptForm.incidentDay

        daysList =
            unassignedIncidentDays scriptForm

        optionList =
            List.map (\i -> Tuple.pair (String.fromInt i) (String.fromInt i)) daysList
    in
    Form.select "-incident-days" chgMsg roleKey optionList


incidentCulprits : (String -> msg) -> RegisterForm -> Html msg
incidentCulprits chgMsg scriptForm =
    let
        roleKey =
            scriptForm.incidentCulprit

        charactersList =
            unassignedCulpritCharacters scriptForm

        optionList =
            List.map (\char -> Tuple.pair (Character.characterToString char) char.name) charactersList
    in
    Form.select "-incident-culprits" chgMsg roleKey optionList


selectTragedySet : (String -> msg) -> RegisterForm -> Html msg
selectTragedySet chgMsg scriptForm =
    select [ onChange chgMsg ]
        [ option [ value <| TragedySet.typeToString TragedySet.FirstSteps, selected (isSetFirstSteps scriptForm) ] [ text <| TragedySet.typeToName TragedySet.FirstSteps ]
        , option [ value <| TragedySet.typeToString TragedySet.BasicTragedy, selected (isSetBasicTragedy scriptForm) ] [ text <| TragedySet.typeToName TragedySet.BasicTragedy ]
        ]


scriptView : Script -> Html msg
scriptView s =
    div []
        [ closeSheet s
        , openSheet s
        ]


closeSheet : Script -> Html msg
closeSheet s =
    div [ class "box" ]
        [ div [ class "title is-5" ]
            [ text "非公開シート"
            ]
        , Form.field
            [ label [ class "label has-text-white" ]
                [ text "脚本名"
                ]
            , div []
                [ text <| Name.toString s.name
                ]
            ]
        , Form.field
            [ label [ class "label has-text-white" ]
                [ text "ルールY"
                ]
            , div []
                [ text s.mainPlot.name
                ]
            ]
        , Form.field
            [ label [ class "label has-text-white" ]
                [ text "ルールX1"
                ]
            , div []
                [ text s.subPlot1.name
                ]
            ]
        , case s.subPlot2 of
            Just p ->
                Form.field
                    [ label [ class "label has-text-white" ]
                        [ text "ルールX2"
                        ]
                    , div []
                        [ text p.name
                        ]
                    ]

            Nothing ->
                text ""
        , Form.field
            [ label [ class "label has-text-white" ]
                [ text "キャラクター"
                ]
            , div []
                (s.characters
                    |> List.reverse
                    -- 選んだ順に表示するため並び替え
                    |> List.map
                        (\c ->
                            Character.characterFormCollectionItem c
                                [ div []
                                    [ -- label [ class "label has-text-white" ] [ text "役職" ]
                                      text <| .name <| Maybe.withDefault TragedySet.person c.role
                                    ]
                                , case c.character.characterType of
                                    Character.TransferStudent ->
                                        div []
                                            [ label [ class "label has-text-white" ] [ text "登場日" ]
                                            , text <| String.fromInt <| Maybe.withDefault 0 c.optionalNumber
                                            ]

                                    Character.GodlyBeing ->
                                        div []
                                            [ label [ class "label has-text-white" ] [ text "登場ループ" ]
                                            , text <| String.fromInt <| Maybe.withDefault 0 c.optionalNumber
                                            ]

                                    Character.Boss ->
                                        div []
                                            [ label [ class "label has-text-white" ] [ text "テリトリー" ]
                                            , text <| .name <| Maybe.withDefault Board.city c.turf
                                            ]

                                    _ ->
                                        text ""
                                ]
                        )
                )
            ]
        , Form.field [ label [ class "label" ] [ text "事件" ] ]
        , Form.field <|
            (s.incidents
                |> List.sortBy .day
                |> List.map
                    (\data ->
                        div [ class "media" ]
                            [ div [ class "media-left", style "padding-left" "1rem", style "align-self" "center" ]
                                [ text <| String.fromInt data.day ++ "日目"
                                ]
                            , div [ class "media-content is-flex" ]
                                [ div [ style "text-align" "center", style "align-self" "center" ] [ text data.incident.name ]
                                , div [ style "padding-left" "1rem" ]
                                    [ img [ src (Character.characterToCardUrl data.culprit) ] []
                                    , div [] [ text <| "" ++ data.culprit.name ]
                                    ]
                                ]
                            ]
                    )
            )
        , Form.field
            [ label [ class "label has-text-white" ] [ text "メモ" ]
            , Form.control
                [ div [ style "white-space" "pre-wrap" ] [ text s.memo ]
                ]
            ]
        ]


openSheet : Script -> Html msg
openSheet s =
    s
        |> scriptToOpenSheet
        |> OpenSheet.openSheetView



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
        , ( "incidents", E.list IncidentScriptData.encode script.incidents )
        , ( "extra", E.string script.extra )
        , ( "memo", E.string script.memo )
        ]
