module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Component.Bulma as Bulma
import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onChange)
import Html.Extra as ExHtml
import Json.Encode exposing (Value)
import Models.Character as Character exposing (Character)
import Models.Room as Room exposing (Room)
import Models.RoomData as RoomData exposing (RoomData)
import Models.RoomData.Board as RoomBoard
import Models.RoomData.Character as RoomCharacter
import Models.RoomData.Hand as Hand exposing (Hand)
import Models.RoomData.OpenSheet as OpenSheet
import Models.RoomName as RoomName exposing (RoomName)
import Models.RoomState as RoomState exposing (RoomState)
import Models.Script as Script exposing (Script)
import Models.Script.IncidentScriptData exposing (IncidentScriptData)
import Models.ScriptName as ScriptName exposing (ScriptName)
import Models.TragedySet as TragedySet
import Models.User exposing (User)
import Ports exposing (..)
import Route
import Task exposing (Task)


main : Program (Maybe User) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { loginUser : Maybe User
    , menuState : MenuState
    , roomForm : Room.RegisterForm
    , room : Maybe Room
    , rooms : Maybe (List RoomName)
    , myRooms : Maybe (List RoomName)
    , scriptForm : Script.RegisterForm
    , script : Maybe Script
    , scripts : Maybe (List ScriptName)
    , roomData : Maybe RoomData
    , roomState : RoomState
    , mainAreaState : MainAreaState
    , modalState : ModalState
    , modalMessage : String
    }


init : Maybe User -> ( Model, Cmd Msg )
init flags =
    let
        firstEvent =
            case flags of
                Nothing ->
                    initLoginUI ()

                _ ->
                    initSwiper ()
    in
    ( initModel flags, firstEvent )


initModel : Maybe User -> Model
initModel flags =
    Model flags MenuClose Room.init Room.initRoom RoomName.initRoomNames RoomName.initRoomNames Script.initForm Script.initScript ScriptName.initScriptNames RoomData.init RoomState.init MainTab CloseModalState ""



-- Msg


type Msg
    = Error String
    | OpenMenu
    | CloseMenu
    | SignOut
    | ChangedUrl String
    | ChangeUrl String
    | OpenModal String
    | CloseModal
      -- 脚本作成
    | ChangeScriptName String
    | UpdateScript
    | ReadedScriptNames Value
    | ReadedScript Value
    | OpenModalConfirmScriptDelete
    | DeleteScript
    | DeletedScript Bool
    | ChangeTragedySet String
    | ChangeMainPlot String
    | ChangeSubPlot1 String
    | ChangeSubPlot2 String
    | AddCharacter Character
    | DeleteCharacter Character
    | ChangeCharacterRole Character.CharacterScriptData String
    | ChangeOptionalNumber Character.CharacterScriptData String
    | ChangeTurf Character.CharacterScriptData String
    | ChangeNumberOfLoops String
    | ChangeDaysInOneLoop String
    | ChangeIncidentCreateFormDay String
    | ChangeIncidentCreateFormCulprit String
    | ChangeIncidentCreateFormIncident String
    | AddIncidents
    | DeleteIncidents IncidentScriptData
    | OpenCharacterSelectModal
    | OpenAddIncidentModal
    | ChangeScriptExtra String
    | ChangeScriptMemo String
    | ChangedScript
      -- ルーム作成
    | ChangeRoomName String
    | ChangeRoomId String
    | UpdateRoom
    | ReadedRooms Value
    | ReadedMyRooms Value
    | ReadedRoom Value
    | ChangeRoomScript String
    | ChangedRoomScript Value
    | ChangeRoomTwitterScreenName PlayerType String
    | ChangedRoom
    | SetIsUseTweet Bool
    | SetIsUseTweetRoomName Bool
      -- ルーム
    | ReadedRoomData Value
    | ReadedRoomForRoomData Value
    | ConfirmInitRoomData
    | InitRoomData
    | ConfirmPublishCloseSheet
    | PublishCloseSheet
    | ChangeRoomDataEx String
    | OpenRoomStateBottomNav
    | CloseRoomStateBottomNav
    | NextRoomDataState
    | UpdateRoomData
    | CharacterRoomDataState
    | DataRoomDataState
    | HandRoomDataState
    | ChangeCharacterGoodWill RoomCharacter.Character String
    | ChangeCharacterParanoia RoomCharacter.Character String
    | ChangeCharacterIntrigue RoomCharacter.Character String
    | ChangeCharacterLocation RoomCharacter.Character String
    | ToggleCharacterIsDead RoomCharacter.Character
    | DeleteCharacterForbiddenLocationMsg RoomCharacter.Character
    | ChangeBoardIntrigue RoomBoard.Board String
    | SetMasterMindHand Int String
    | SetMasterMindOnComponent Int String
    | SetProtagonistHand Int String
    | SetProtagonistOnComponent Int String
    | ResolveCards
    | ConfirmLoopEnd
    | LoopEnd
    | ConfirmHandUnused Int Hand
    | HandUnused Int Hand
    | ToggleCharacterIsSetEx RoomCharacter.Character
    | OpenTwitterModal
    | OpenCharacterCardModal Character.CharacterType


type MenuState
    = MenuClose
    | MenuOpen


type MainAreaState
    = MainTab
    | ScriptTab
    | NothingTab
    | ScriptCreateTab
    | RoomEditTab
    | RoomTab


type ModalState
    = OpenModalState
    | CloseModalState
    | ConfirmModalState String Msg
    | CharacterSelectModalState
    | OpenAddIncidentModalState
    | OpenTwitterModalState
    | OpenCharacterCardModalState Character.CharacterType


type PlayerType
    = Mastermind
    | Protagonist1
    | Protagonist2
    | Protagonist3



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Error errorMessage ->
            ( model, errorMessage |> errorToJs )

        OpenMenu ->
            ( { model | menuState = MenuOpen }, Cmd.none )

        CloseMenu ->
            ( { model | menuState = MenuClose }, Cmd.none )

        SignOut ->
            ( model, signOut () )

        OpenModal message ->
            ( { model | modalState = OpenModalState, modalMessage = message }, Cmd.none )

        CloseModal ->
            ( { model | modalState = CloseModalState }, Cmd.none )

        OpenModalConfirmScriptDelete ->
            ( { model | modalState = ConfirmModalState "削除" DeleteScript }, Cmd.none )

        ChangeRoomName name ->
            ( { model | roomForm = Room.setName name model.roomForm }, Cmd.none )

        ChangeUrl url ->
            ( model, changeUrl url )

        ChangedUrl url ->
            case Route.toRoute url of
                Route.Top ->
                    ( { model | mainAreaState = MainTab, script = Nothing, roomData = Nothing, room = Nothing }, readRooms () )

                Route.Script ->
                    ( { model | mainAreaState = ScriptTab }, readScriptNames () )

                Route.ScriptCreate ->
                    ( { model | mainAreaState = ScriptCreateTab, scriptForm = Script.initForm }, Cmd.none )

                Route.ScriptEdit s ->
                    ( { model | mainAreaState = ScriptCreateTab }, readScript s )

                Route.RoomEdit s ->
                    ( { model | mainAreaState = RoomEditTab }, Cmd.batch [ readScriptNames (), readRoom s ] )

                Route.Room s ->
                    ( { model | mainAreaState = RoomTab, room = Nothing, roomData = Nothing }, Cmd.batch [ listenRoomData s ] )

                Route.NotFound ->
                    update (OpenModal ("指定されたURLが見つかりません。\nご確認お願いします。\n" ++ url)) { model | mainAreaState = NothingTab }

        -- 脚本作成画面
        ChangeScriptName name ->
            update ChangedScript { model | scriptForm = Script.setName name model.scriptForm }

        ReadedScriptNames val ->
            -- 脚本更新後にここに飛ぶため、入力欄を初期化
            ( { model | scripts = ScriptName.decodeScriptNameListFromJson val, scriptForm = Script.initForm }, Cmd.none )

        UpdateScript ->
            let
                script =
                    Script.convert model.scriptForm
            in
            case script of
                Nothing ->
                    update (OpenModal "保存に失敗しました。項目を再確認してください") { model | script = script }

                Just s ->
                    ( { model | script = script }, updateScript <| Script.encode s )

        ReadedScript val ->
            let
                registerForm =
                    Script.decodeScriptRegisterFormFromJson val
            in
            case registerForm of
                Just f ->
                    ( { model | scriptForm = f, script = Script.convert f }, Cmd.none )

                Nothing ->
                    update (OpenModal "脚本の読み込みに失敗しました。一度トップに戻ります。") { model | mainAreaState = MainTab }

        DeleteScript ->
            ( model, deleteScript model.scriptForm.id )

        DeletedScript result ->
            if result then
                ( { model | mainAreaState = ScriptTab, modalState = CloseModalState }, readScriptNames () )

            else
                update (OpenModal "脚本の削除に失敗しました。一度トップに戻ります。") { model | mainAreaState = MainTab }

        ChangeTragedySet val ->
            update ChangedScript { model | scriptForm = Script.setTragedySet val model.scriptForm }

        ChangeMainPlot val ->
            update ChangedScript { model | scriptForm = Script.setMainPlot val model.scriptForm }

        ChangeSubPlot1 val ->
            update ChangedScript { model | scriptForm = Script.setSubPlot1 val model.scriptForm }

        ChangeSubPlot2 val ->
            update ChangedScript { model | scriptForm = Script.setSubPlot2 val model.scriptForm }

        ChangeCharacterRole char val ->
            update ChangedScript { model | scriptForm = Script.setCharacterRole char val model.scriptForm }

        OpenCharacterSelectModal ->
            ( { model | modalState = CharacterSelectModalState }, Cmd.none )

        AddCharacter c ->
            update ChangedScript { model | scriptForm = Script.setCharacter c model.scriptForm }

        DeleteCharacter c ->
            update ChangedScript { model | scriptForm = Script.deleteCharacter c model.scriptForm }

        ChangeOptionalNumber char val ->
            update ChangedScript { model | scriptForm = Script.setCharacterOptionalNumber char val model.scriptForm }

        ChangeTurf char val ->
            update ChangedScript { model | scriptForm = Script.setCharacterTurf char val model.scriptForm }

        ChangeNumberOfLoops val ->
            update ChangedScript { model | scriptForm = Script.setNumberOfLoops val model.scriptForm }

        ChangeDaysInOneLoop val ->
            update ChangedScript { model | scriptForm = Script.setDaysInOneLoop val model.scriptForm }

        OpenAddIncidentModal ->
            case Script.unassignedIncidentDays model.scriptForm of
                [] ->
                    update (OpenModal "既に全ての日に事件が割り振られています。") model

                day :: _ ->
                    case Script.unassignedCulpritCharacters model.scriptForm of
                        [] ->
                            -- TODO: 選択中のキャラクターが0人のときにも出る。
                            update (OpenModal "既にキャラクターに事件が割り振られています。") model

                        char :: _ ->
                            case List.head model.scriptForm.set.incidents of
                                Nothing ->
                                    update (OpenModal "ここに来ることはありえません") model

                                Just incident ->
                                    let
                                        scriptForm =
                                            model.scriptForm
                                                |> Script.setIntIncidentDay day
                                                |> Script.setIncidentCulprit (Character.characterToString char)
                                                |> Script.setIncident (TragedySet.incidentToString incident)
                                    in
                                    ( { model | modalState = OpenAddIncidentModalState, scriptForm = scriptForm }, Cmd.none )

        ChangeIncidentCreateFormDay val ->
            ( { model | scriptForm = Script.setIncidentDay val model.scriptForm }, Cmd.none )

        ChangeIncidentCreateFormCulprit val ->
            ( { model | scriptForm = Script.setIncidentCulprit val model.scriptForm }, Cmd.none )

        ChangeIncidentCreateFormIncident val ->
            ( { model
                -- 連続殺人→他の事件に変えた時にキャラクターが切り替わらない対策を含める
                | scriptForm =
                    model.scriptForm
                        |> Script.setIncident val
                        |> (\form ->
                                if List.member (Character.characterFromString model.scriptForm.incidentCulprit |> Maybe.withDefault Character.boyStudent) (Script.unassignedCulpritCharacters form) then
                                    form

                                else
                                    case Script.unassignedCulpritCharacters form of
                                        [] ->
                                            form

                                        char :: _ ->
                                            form |> Script.setIncidentCulprit (Character.characterToString char)
                           )
              }
            , Cmd.none
            )

        AddIncidents ->
            update ChangedScript { model | scriptForm = Script.addIncidents model.scriptForm, modalState = CloseModalState }

        DeleteIncidents val ->
            update ChangedScript { model | scriptForm = Script.deleteIncidents val model.scriptForm }

        ChangeScriptExtra val ->
            update ChangedScript { model | scriptForm = Script.setExtra val model.scriptForm }

        ChangeScriptMemo val ->
            update ChangedScript { model | scriptForm = Script.setMemo val model.scriptForm }

        ChangedScript ->
            ( { model | script = Script.convert model.scriptForm }, Cmd.none )

        -- 部屋編集画面
        ChangeRoomId id ->
            update ChangedRoom { model | roomForm = Room.setId id model.roomForm }

        UpdateRoom ->
            case Room.convert model.roomForm of
                Nothing ->
                    update (OpenModal "保存に失敗しました。項目を再確認してください") { model | room = Nothing }

                Just r ->
                    ( { model | room = Just r }, updateRoom <| Room.encode r )

        ReadedRooms val ->
            ( { model | rooms = RoomName.decodeRoomNameListFromJson val }, Cmd.none )

        ReadedMyRooms val ->
            ( { model | myRooms = RoomName.decodeRoomNameListFromJson val }, Cmd.none )

        ReadedRoom val ->
            let
                registerForm =
                    Room.decodeRegisterFormFromJson val
            in
            case registerForm of
                Just f ->
                    let
                        -- 脚本家の初期値をユーザのTwitterIDに変更
                        mastermindTwitterScreenName =
                            if f.mastermindTwitterScreenName == "" then
                                case model.loginUser of
                                    Just user ->
                                        user.twitterScreenName

                                    Nothing ->
                                        ""

                            else
                                f.mastermindTwitterScreenName

                        newF =
                            { f | mastermindTwitterScreenName = mastermindTwitterScreenName }

                        newCmd =
                            case f.script of
                                Just s ->
                                    readScriptForRoom <| Script.getId s

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model | roomForm = newF, room = Room.convert newF }, newCmd )

                Nothing ->
                    update (OpenModal "部屋の読み込みに失敗しました。一度トップに戻ります。") { model | mainAreaState = MainTab }

        ChangeRoomScript s ->
            if s == "未選択" then
                update ChangedRoom { model | roomForm = Room.setScript Nothing <| Room.setScriptId s <| model.roomForm }

            else
                ( { model | roomForm = Room.setScriptId s model.roomForm }, readScriptForRoom s )

        ChangedRoomScript val ->
            let
                script =
                    Script.decodeScriptFromJson val
            in
            update ChangedRoom { model | roomForm = Room.setScript script model.roomForm }

        ChangeRoomTwitterScreenName pltype s ->
            case pltype of
                Mastermind ->
                    update ChangedRoom { model | roomForm = Room.setMastermindTwitterScreenName s model.roomForm }

                Protagonist1 ->
                    update ChangedRoom { model | roomForm = Room.setProtagonist1TwitterScreenName s model.roomForm }

                Protagonist2 ->
                    update ChangedRoom { model | roomForm = Room.setProtagonist2TwitterScreenName s model.roomForm }

                Protagonist3 ->
                    update ChangedRoom { model | roomForm = Room.setProtagonist3TwitterScreenName s model.roomForm }

        SetIsUseTweet b ->
            update ChangedRoom { model | roomForm = Room.setIsUseTweet b model.roomForm }

        SetIsUseTweetRoomName b ->
            update ChangedRoom { model | roomForm = Room.setIsUseTweetRoomName b model.roomForm }

        ChangedRoom ->
            ( { model | room = Room.convert model.roomForm }, Cmd.none )

        -- ルーム
        OpenRoomStateBottomNav ->
            ( { model | roomState = RoomState.setBottomNav True model.roomState }, Cmd.none )

        CloseRoomStateBottomNav ->
            ( { model | roomState = RoomState.setBottomNav False model.roomState }, Cmd.none )

        ReadedRoomData val ->
            case RoomData.decode val of
                Just data ->
                    ( { model | roomData = Just data, roomState = RoomState.updateByRoomDataState (Just data) <| RoomState.updateTweetProtagonistNumber (Just data) model.loginUser <| model.roomState }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ReadedRoomForRoomData val ->
            case Room.decodeRoomFromJson val of
                Just s ->
                    ( { model | room = Just s }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ConfirmInitRoomData ->
            ( { model | modalState = ConfirmModalState "初期化" InitRoomData }, Cmd.none )

        InitRoomData ->
            case model.room of
                Just r ->
                    let
                        roomData =
                            RoomData.initRoomData r
                    in
                    ( { model | roomData = Just roomData, modalState = CloseModalState, roomState = RoomState.setParameterTab model.roomState }, Cmd.batch [ updateRoomData (RoomData.encode roomData) ] )

                Nothing ->
                    update (OpenModal "部屋の読み込みに失敗しました。一度トップに戻ります。") { model | mainAreaState = MainTab, modalState = CloseModalState }

        ConfirmPublishCloseSheet ->
            ( { model | modalState = ConfirmModalState "公開" PublishCloseSheet }, Cmd.none )

        PublishCloseSheet ->
            let
                roomData =
                    model.roomData |> Maybe.map (model.room |> Maybe.map .script |> RoomData.setScript)
            in
            update UpdateRoomData { model | roomData = roomData, modalState = CloseModalState }

        ChangeRoomDataEx val ->
            ( { model | roomData = model.roomData |> Maybe.map (RoomData.setEx val) }, Cmd.none )

        NextRoomDataState ->
            let
                roomData =
                    model.roomData |> Maybe.map RoomData.nextRoomDataState

                modalState =
                    case model.roomData of
                        Just d ->
                            if RoomData.isOpenTweetModal d && d.isUseTweet then
                                OpenTwitterModalState

                            else
                                model.modalState

                        Nothing ->
                            model.modalState
            in
            update UpdateRoomData { model | roomData = roomData, roomState = RoomState.updateByRoomDataState roomData <| RoomState.updateTweetProtagonistNumber roomData model.loginUser <| model.roomState, modalState = modalState }

        UpdateRoomData ->
            let
                command =
                    case model.roomData of
                        Just d ->
                            updateRoomData (RoomData.encode d)

                        Nothing ->
                            Cmd.none
            in
            ( model, Cmd.batch [ command ] )

        CharacterRoomDataState ->
            ( { model | roomState = RoomState.setParameterTab model.roomState }, Cmd.none )

        DataRoomDataState ->
            ( { model | roomState = RoomState.setActionTab model.roomState }, Cmd.none )

        HandRoomDataState ->
            ( { model | roomState = RoomState.setHandTab model.roomState }, Cmd.none )

        ChangeCharacterGoodWill c s ->
            ( { model | roomData = Maybe.map (RoomData.changeCharacterGoodWill c s) model.roomData }, Cmd.none )

        ChangeCharacterParanoia c s ->
            ( { model | roomData = Maybe.map (RoomData.changeCharacterParanoia c s) model.roomData }, Cmd.none )

        ChangeCharacterIntrigue c s ->
            ( { model | roomData = Maybe.map (RoomData.changeCharacterIntrigue c s) model.roomData }, Cmd.none )

        ChangeCharacterLocation c s ->
            ( { model | roomData = Maybe.map (RoomData.changeCharacterLocation c s) model.roomData }, Cmd.none )

        ToggleCharacterIsDead c ->
            ( { model | roomData = Maybe.map (RoomData.toggleCharacterIsDead c) model.roomData }, Cmd.none )

        DeleteCharacterForbiddenLocationMsg c ->
            ( { model | roomData = Maybe.map (RoomData.deleteForbiddenLocation c) model.roomData }, Cmd.none )

        ChangeBoardIntrigue b s ->
            ( { model | roomData = Maybe.map (RoomData.changeBoardIntrigue b s) model.roomData }, Cmd.none )

        SetMasterMindHand i s ->
            ( { model | roomData = Maybe.map (RoomData.changeMasterMindHand i s) model.roomData }, Cmd.none )

        SetMasterMindOnComponent i s ->
            ( { model | roomData = Maybe.map (RoomData.changeMasterMindComponent i s) model.roomData }, Cmd.none )

        SetProtagonistHand i s ->
            ( { model | roomData = Maybe.map (RoomData.changeProtagonistHand i s) model.roomData }, Cmd.none )

        SetProtagonistOnComponent i s ->
            ( { model | roomData = Maybe.map (RoomData.changeProtagonistComponent i s) model.roomData }, Cmd.none )

        ResolveCards ->
            ( { model | roomData = Maybe.map RoomData.resolveCards model.roomData, roomState = RoomState.setFalseIsDisplayCardsAreResolved model.roomState }, Cmd.none )

        ConfirmLoopEnd ->
            ( { model | modalState = ConfirmModalState "ループ終了" LoopEnd }, Cmd.none )

        LoopEnd ->
            let
                roomData =
                    Maybe.map RoomData.loopEnd model.roomData
            in
            update UpdateRoomData { model | roomData = roomData, modalState = CloseModalState, roomState = RoomState.updateByRoomDataState roomData model.roomState }

        ConfirmHandUnused i h ->
            if isRoomOwner model then
                case h.isUsed of
                    Just True ->
                        ( { model | modalState = ConfirmModalState (Hand.toName h ++ "を回収") (HandUnused i h) }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        HandUnused i h ->
            ( { model | roomData = Maybe.map (RoomData.unusedProtagonistHand i h) model.roomData, modalState = CloseModalState }, Cmd.none )

        ToggleCharacterIsSetEx c ->
            ( { model | roomData = Maybe.map (RoomData.toggleCharacterIsSetEx c) model.roomData }, Cmd.none )

        OpenTwitterModal ->
            ( { model | modalState = OpenTwitterModalState }, Cmd.none )

        OpenCharacterCardModal t ->
            ( { model | modalState = OpenCharacterCardModalState t }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ readedRooms ReadedRooms
        , readedMyRooms ReadedMyRooms
        , changedUrl ChangedUrl
        , readedScriptNames ReadedScriptNames
        , readedScript ReadedScript
        , deletedScript DeletedScript
        , readedRoom ReadedRoom
        , readedScriptForRoom ChangedRoomScript
        , readedRoomData ReadedRoomData
        , readedRoomForRoomData ReadedRoomForRoomData
        ]


view : Model -> Html Msg
view model =
    div [ class "rooper-container" ]
        [ nav [ class "rooper-page-head" ]
            [ headNavLeft model
            , headNavRight model
            ]
        , main_ [ class "rooper-main" ] [ mainContent model ]
        , modal model
        ]


headNavLeft : Model -> Html msg
headNavLeft model =
    div [ class "left" ]
        [ case model.roomData of
            Nothing ->
                a [ href "/rooper/", style "color" "red" ] [ text "惨劇RoopeR online tool" ]

            Just data ->
                case model.loginUser of
                    Nothing ->
                        a [ href "/", style "color" "red" ] [ text "惨劇RoopeR online tool" ]

                    Just user ->
                        RoomData.tags (Just data) user
        ]


mainContent : Model -> Html Msg
mainContent model =
    case model.loginUser of
        Nothing ->
            case model.mainAreaState of
                RoomTab ->
                    notOwnerRoomView model

                _ ->
                    mainContentBox model

        Just u ->
            case model.mainAreaState of
                ScriptTab ->
                    mainContentBox model

                ScriptCreateTab ->
                    div [ class "content" ] [ createScriptView model ]

                RoomEditTab ->
                    div [ class "content" ] [ editRoomView model ]

                MainTab ->
                    mainContentBox model

                RoomTab ->
                    loginedUserRoomView u model

                NothingTab ->
                    ExHtml.nothing


loginedUserRoomView : User -> Model -> Html Msg
loginedUserRoomView u model =
    if isRoomOwner model then
        ownerRoomView u model

    else
        notOwnerRoomView model


isRoomOwner : Model -> Bool
isRoomOwner { room } =
    case room of
        Just _ ->
            True

        Nothing ->
            False


ownerRoomView : User -> Model -> Html Msg
ownerRoomView user model =
    case model.roomData of
        Nothing ->
            Form.createButton InitRoomData "ルーム初期化"

        Just data ->
            div []
                [ RoomData.infos data
                , RoomData.stateView data
                , RoomData.roomBoard data
                , RoomData.playedHandsView data
                , RoomData.usedHands ConfirmHandUnused data
                , mastermindSheets model
                , if RoomData.isTurnProtagonist model.roomState.turnProtagonistNumber user data then
                    protagonistsBottomForm model user data

                  else if RoomData.isDisplayMastermindBottomForm user data then
                    mastermindBottomForm model data

                  else
                    ExHtml.nothing

                -- , creativeCommmons
                ]


mastermindSheets : Model -> Html Msg
mastermindSheets model =
    section [ class "section" ]
        [ case model.room of
            Just room ->
                Script.scriptView OpenCharacterCardModal room.script

            Nothing ->
                ExHtml.nothing
        ]


mastermindScriptButtons : Model -> RoomData -> Html Msg
mastermindScriptButtons model data =
    div [ class "box" ]
        [ case data.script of
            Nothing ->
                Form.field
                    [ button [ class "button is-danger", onClick ConfirmPublishCloseSheet ]
                        [ span [ class "icon" ]
                            [ i [ class "fas fa-book" ] []
                            ]
                        , span [] [ text "非公開シートを公開..." ]
                        ]
                    ]

            Just _ ->
                Form.field
                    [ button [ class "button is-info", disabled True ]
                        [ span [ class "icon" ]
                            [ i [ class "fas fa-book" ] []
                            ]
                        , span [] [ text "非公開シート公開済" ]
                        ]
                    ]
        , Form.field
            [ button [ class "button is-danger", onClick ConfirmInitRoomData ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-book" ] []
                    ]
                , span [] [ text "ルーム初期化..." ]
                ]
            ]
        ]


mastermindBottomForm : Model -> RoomData -> Html Msg
mastermindBottomForm model data =
    RoomState.roomDataBottomForm model.roomState
        [ header
            [ class "card-header" ]
            [ RoomState.roomDataFormHeaderTitle model.roomState
            , RoomState.roomDataFormHeaderIcon CloseRoomStateBottomNav OpenRoomStateBottomNav model.roomState
            ]
        , RoomState.roomDataFormContent
            [ case model.roomState.tabsState of
                RoomState.Parameter ->
                    div []
                        [ case data.openSheet.set.setType of
                            TragedySet.BasicTragedy ->
                                ExHtml.nothing

                            TragedySet.FirstSteps ->
                                ExHtml.nothing

                            TragedySet.MysteryCircle ->
                                RoomState.roomDataFormDataBoard
                                    [ --  td [] [ input [ class "input", type_ "number", onChange ChangeRoomDataLoop, value <| String.fromInt data.loop ] [] ]
                                      -- , td [] [ input [ class "input", type_ "number", onChange ChangeRoomDataDate, value <| String.fromInt data.date ] [] ]
                                      td [] [ input [ class "input", type_ "number", onChange ChangeRoomDataEx, value <| String.fromInt data.ex ] [] ]
                                    ]
                        , RoomData.boardsForm data ChangeBoardIntrigue
                        , RoomData.charactersForm data ChangeCharacterLocation ChangeCharacterGoodWill ChangeCharacterParanoia ChangeCharacterIntrigue ToggleCharacterIsDead DeleteCharacterForbiddenLocationMsg ToggleCharacterIsSetEx
                        ]

                RoomState.Action ->
                    div []
                        [ div [ class "box" ]
                            [ button [ class "button is-primary", onClick ConfirmLoopEnd ]
                                [ span [ class "icon" ] [ i [ class "fas fa-clock" ] [] ]
                                , span [] [ text "ループを終了させる..." ]
                                ]
                            ]
                        , mastermindScriptButtons model data
                        ]

                RoomState.Hand ->
                    div [ class "rooper-mastermind-form-hands" ]
                        [ RoomData.handsFormMastermind 1 data (SetMasterMindHand 1) (SetMasterMindOnComponent 1)
                        , RoomData.handsFormMastermind 2 data (SetMasterMindHand 2) (SetMasterMindOnComponent 2)
                        , RoomData.handsFormMastermind 3 data (SetMasterMindHand 3) (SetMasterMindOnComponent 3)
                        ]
            ]
        , RoomState.roomDataFormFooter
            [ span [ class "card-footer-item" ]
                [ if RoomData.isMastermindPlaysCards data || RoomData.isProtagonistsPlaysCard data then
                    span [ class "card-footer-item", onClick HandRoomDataState ]
                        [ span [] [ text "手札" ]
                        ]

                  else
                    nextStateButton
                ]
            , span [ class "card-footer-item", onClick CharacterRoomDataState ]
                [ span [] [ text "パラメータ" ]
                ]
            , span [ class "card-footer-item", onClick DataRoomDataState ]
                [ span [] [ text "アクション" ]
                ]
            ]
        , if RoomData.isMastermindHandsPlayed data then
            RoomState.roomDataFormFooter [ span [ class "card-footer-item" ] [ nextStateButton ] ]

          else if model.roomState.isDisplayCardsAreResolved then
            RoomState.roomDataFormFooter
                [ span [ class "card-footer-item" ]
                    [ button [ class "button is-primary", onClick ResolveCards ]
                        [ span [] [ text "手札反映" ]
                        ]
                    ]
                ]

          else
            ExHtml.nothing
        ]


protagonistsBottomForm : Model -> User -> RoomData -> Html Msg
protagonistsBottomForm { roomState } user data =
    RoomState.roomDataBottomForm roomState
        [ header
            [ class "card-header" ]
            [ RoomState.roomDataFormHeaderTitle roomState
            , RoomState.roomDataFormHeaderIcon CloseRoomStateBottomNav OpenRoomStateBottomNav roomState
            ]
        , RoomState.roomDataFormContent
            [ div [ class "rooper-mastermind-form-hands" ]
                [ RoomData.handsFormProtagonist roomState.turnProtagonistNumber user data (SetProtagonistHand roomState.turnProtagonistNumber) (SetProtagonistOnComponent roomState.turnProtagonistNumber)
                ]
            ]
        , RoomState.roomDataFormFooter
            [ span [ class "card-footer-item" ]
                [ span [ class "card-footer-item", onClick HandRoomDataState ]
                    [ span [] [ text "手札" ]
                    ]
                ]
            ]
        , if RoomData.isProtagonistHandsPlayed roomState.turnProtagonistNumber data then
            RoomState.roomDataFormFooter [ span [ class "card-footer-item" ] [ nextStateButton ] ]

          else
            ExHtml.nothing
        ]


nextStateButton : Html Msg
nextStateButton =
    button [ class "button is-primary", onClick NextRoomDataState ]
        [ span [] [ text "Next" ]
        , span [ class "icon" ]
            [ i [ class "fas fa-arrow-right" ] []
            ]
        ]


notOwnerRoomView : Model -> Html Msg
notOwnerRoomView model =
    case model.roomData of
        Nothing ->
            text "まだルームが作成されていません"

        Just data ->
            case model.loginUser of
                Nothing ->
                    notLoginedUserRoomView data

                Just user ->
                    userRoomView model user data


userRoomView : Model -> User -> RoomData -> Html Msg
userRoomView model user data =
    div []
        [ RoomData.infos data
        , RoomData.stateView data
        , RoomData.roomBoard data
        , RoomData.playedHandsView data
        , RoomData.usedHands ConfirmHandUnused data
        , RoomData.roomDataView OpenCharacterCardModal data
        , RoomData.openSheetView data
        , RoomData.closeSheetView OpenCharacterCardModal data
        , if RoomData.isTurnProtagonist model.roomState.turnProtagonistNumber user data then
            protagonistsBottomForm model user data

          else
            ExHtml.nothing

        -- , creativeCommmons
        ]


notLoginedUserRoomView : RoomData -> Html Msg
notLoginedUserRoomView data =
    div []
        [ RoomData.infos data
        , RoomData.stateView data
        , RoomData.roomBoard data
        , RoomData.usedHands ConfirmHandUnused data
        , RoomData.roomDataView OpenCharacterCardModal data
        , RoomData.openSheetView data
        , RoomData.closeSheetView OpenCharacterCardModal data

        -- , creativeCommmons
        ]


mainContentBox : Model -> Html Msg
mainContentBox model =
    div []
        [ div [ class "center box" ]
            [ mainTabs model
            , mainMessage model
            ]

        -- , div [ style "position" "absolute", style "bottom" "0" ]
        --     [ creativeCommmons
        --     ]
        ]


modal : Model -> Html Msg
modal model =
    let
        { modalMessage, modalState } =
            model

        isActive =
            case modalState of
                CloseModalState ->
                    False

                _ ->
                    True
    in
    Bulma.modal isActive CloseModal <|
        div [ class "box rooper-modal-message" ]
            [ case modalState of
                ConfirmModalState confirmText message ->
                    div []
                        [ div [ class "columns is-mobile" ]
                            [ text <| confirmText ++ "します。よろしいですか？"
                            ]
                        , div [ class "columns is-mobile" ]
                            [ div [ class "column  is-4  is-offset-1 control" ]
                                [ button [ class "button is-danger", onClick message ] [ text confirmText ]
                                ]
                            , div [ class "column  is-4 is-offset-2 control" ]
                                [ button [ class "button is-info", onClick CloseModal ] [ text "キャンセル" ] ]
                            ]
                        ]

                OpenModalState ->
                    text modalMessage

                CloseModalState ->
                    text modalMessage

                CharacterSelectModalState ->
                    div [ style "display" "flex", style "flex-wrap" "wrap" ]
                        (List.map (\c -> characterNameCard c model) Character.characters)

                OpenAddIncidentModalState ->
                    div []
                        [ Form.field
                            [ label [ class "label has-text-white" ] [ text "事件予定日" ]
                            , div [ class "select" ] [ Script.incidentDays ChangeIncidentCreateFormDay model.scriptForm ]
                            ]
                        , Form.field
                            [ label [ class "label has-text-white" ] [ text "事件" ]
                            , div [ class "select" ] [ Script.incidentsSelect ChangeIncidentCreateFormIncident model.scriptForm ]
                            ]
                        , Form.field
                            [ label [ class "label has-text-white" ] [ text "犯人" ]
                            , div [ class "select" ] [ Script.incidentCulprits ChangeIncidentCreateFormCulprit model.scriptForm ]
                            ]
                        , Form.field [ button [ class "button is-info", onClick AddIncidents ] [ text "追加" ] ]
                        ]

                OpenTwitterModalState ->
                    case model.roomData of
                        Just d ->
                            case model.loginUser of
                                Just u ->
                                    RoomData.tweetView d u model.roomState.tweetProtagonistNumber CloseModal

                                Nothing ->
                                    ExHtml.nothing

                        Nothing ->
                            ExHtml.nothing

                OpenCharacterCardModalState t ->
                    div [ style "text-align" "center" ]
                        [ img [ src <| Character.characterTypeToLargeCardUrl t ] []
                        ]
            ]


characterNameCard : Character.Character -> Model -> Html Msg
characterNameCard c model =
    let
        isSelected =
            Script.containCharacter c model.scriptForm

        clickMessage =
            if isSelected then
                DeleteCharacter c

            else
                AddCharacter c
    in
    Character.characterNameCard clickMessage c isSelected


mainTabs : Model -> Html msg
mainTabs model =
    let
        { mainAreaState, loginUser } =
            model

        mainTabClass =
            case mainAreaState of
                MainTab ->
                    class "is-active"

                _ ->
                    class ""

        scriptTabClass =
            case mainAreaState of
                ScriptTab ->
                    class "is-active"

                ScriptCreateTab ->
                    class "is-active"

                _ ->
                    class ""
    in
    case loginUser of
        Nothing ->
            ExHtml.nothing

        Just _ ->
            div [ class "tabs" ]
                [ ul []
                    [ li [ mainTabClass ] [ a [ href "/rooper/" ] [ text "メイン" ] ]
                    , li [ scriptTabClass ] [ a [ href "/rooper/script/" ] [ text "脚本" ] ]
                    ]
                ]


mainMessage : Model -> Html Msg
mainMessage model =
    case model.loginUser of
        Just _ ->
            logginedMainArea model

        Nothing ->
            loginMessage


logginedMainArea : Model -> Html Msg
logginedMainArea model =
    case model.mainAreaState of
        MainTab ->
            mainTopContent model

        NothingTab ->
            ExHtml.nothing

        ScriptTab ->
            mainScriptContent model

        ScriptCreateTab ->
            createScriptView model

        RoomTab ->
            text "ルーム建設予定地"

        RoomEditTab ->
            editRoomView model


mainScriptContent : Model -> Html Msg
mainScriptContent model =
    let
        { scripts } =
            model
    in
    div []
        [ div [ class "columns is-mobile" ]
            [ div [ class "column is-5 is-offset-7" ]
                [ Form.createButton (ChangeUrl "/rooper/script/create/") "create"
                ]
            ]
        , case scripts of
            Just [] ->
                text "脚本はまだ作られていません"

            Just s ->
                ScriptName.scripts s

            _ ->
                ExHtml.nothing
        ]


mainTopContent : Model -> Html Msg
mainTopContent model =
    let
        { rooms, myRooms } =
            model
    in
    div []
        [ case myRooms of
            Just r ->
                RoomName.roomEdits r

            Nothing ->
                ExHtml.nothing
        , case rooms of
            Just r ->
                RoomName.rooms r

            Nothing ->
                ExHtml.nothing
        ]


editRoomView : Model -> Html Msg
editRoomView { roomForm, scripts, room } =
    let
        isRoomInvalid =
            case room of
                Just _ ->
                    False

                Nothing ->
                    True
    in
    Room.registerForm
        [ Form.field
            [ label [ class "label has-text-white" ]
                [ text "ルーム名"
                ]
            , Form.control
                [ input [ class "input", required True, onInput ChangeRoomName, value roomForm.name ] []
                ]
            , Form.errors (Room.getNameError roomForm)
            ]
        , Form.field
            [ label [ class "label has-text-white" ] [ text "脚本" ]
            , case scripts of
                Just list ->
                    div [ class "select" ] [ Room.scriptSelectForm ChangeRoomScript list roomForm ]

                Nothing ->
                    text "脚本がありません。先に作成してください"
            , Form.errors
                [ ( "脚本を選択してください", List.member Room.RequiredScript (Room.errors roomForm) )
                ]
            ]
        , div [ style "display" "none" ]
            [ Form.field
                [ label [ class "label has-text-white" ]
                    [ text "脚本家TwitterID"
                    ]
                , Form.control
                    [ input [ class "input", required True, onInput (ChangeRoomTwitterScreenName Mastermind), value roomForm.mastermindTwitterScreenName ] []
                    ]
                , Form.errors
                    [ ( "脚本家IDを入力してください", List.member Room.RequiredMastermindTwitterScreenName (Room.errors roomForm) )
                    ]
                ]
            ]
        , Form.field
            [ label [ class "label has-text-white" ]
                [ text "主人公1TwitterID"
                ]
            , Form.control
                [ input [ class "input", required True, onInput (ChangeRoomTwitterScreenName Protagonist1), pattern "[^@]*", value roomForm.protagonist1TwitterScreenName ] []
                ]
            , Form.errors
                [ ( "主人公IDを入力してください(IDに@は不要)", List.member Room.RequiredProtagonist1TwitterScreenName (Room.errors roomForm) )
                ]
            ]
        , Form.field
            [ label [ class "label has-text-white" ]
                [ text "主人公2TwitterID"
                ]
            , Form.control
                [ input [ class "input", required True, onInput (ChangeRoomTwitterScreenName Protagonist2), value roomForm.protagonist2TwitterScreenName ] []
                ]
            , Form.errors
                [ ( "主人公IDを入力してください", List.member Room.RequiredProtagonist2TwitterScreenName (Room.errors roomForm) )
                ]
            ]
        , Form.field
            [ label [ class "label has-text-white" ]
                [ text "主人公3TwitterID"
                ]
            , Form.control
                [ input [ class "input", required True, onInput (ChangeRoomTwitterScreenName Protagonist3), value roomForm.protagonist3TwitterScreenName ] []
                ]
            , Form.errors
                [ ( "主人公IDを入力してください", List.member Room.RequiredProtagonist3TwitterScreenName (Room.errors roomForm) )
                ]
            ]
        , Form.field
            [ label [ class "label has-text-white" ]
                [ text "呟きボタンを表示"
                ]
            , Form.control
                [ input [ class "", type_ "checkbox", checked roomForm.isUseTweet, onClick (SetIsUseTweet <| not roomForm.isUseTweet) ] []
                ]
            ]
        , Form.field
            [ label [ class "label has-text-white" ]
                [ text "呟きにルーム名を含める"
                ]
            , Form.control
                [ input [ class "", type_ "checkbox", checked roomForm.isUseTweetRoomName, onClick (SetIsUseTweetRoomName <| not roomForm.isUseTweetRoomName) ] []
                ]
            ]
        , Form.field
            [ div [ class "control" ]
                [ button [ class "button is-primary", disabled isRoomInvalid, onClick UpdateRoom ] [ text "更新" ]
                ]
            ]
        , div []
            [ div [] [ text "選択中の脚本" ]
            ]
        , case roomForm.script of
            Just s ->
                Script.scriptView OpenCharacterCardModal s

            Nothing ->
                ExHtml.nothing
        , Form.field
            [ button [ class "button is-danger", onClick ConfirmInitRoomData ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-book" ] []
                    ]
                , span [] [ text "ルーム初期化..." ]
                ]
            ]
        ]



-- ログインメッセージ


loginMessage : Html msg
loginMessage =
    div [ class "login-message" ]
        [ p []
            [ text "ようこそ惨劇オンラインへ。" ]
        , p [ id "login-message-text" ]
            [ text "まずはログインしてください。" ]
        , div
            [ id "firebaseui-auth-container" ]
            []
        ]



-- メニュー


headNavRight : Model -> Html Msg
headNavRight model =
    case model.loginUser of
        Just user ->
            let
                menuClass =
                    case model.menuState of
                        MenuClose ->
                            "menu"

                        MenuOpen ->
                            "menu open"

                clickEvent =
                    case model.menuState of
                        MenuClose ->
                            OpenMenu

                        MenuOpen ->
                            CloseMenu
            in
            div [ class "right", onClick clickEvent ]
                [ div [ class menuClass ]
                    [ ul []
                        [ li [ onClick (ChangeUrl "/rooper/") ] [ text "マイページ" ]
                        , li [ onClick SignOut ] [ text "サインアウト" ]
                        ]
                    ]
                , img [ src user.twitterProfileImageUrl ] []
                ]

        Nothing ->
            ExHtml.nothing



-- 脚本


createScriptView : Model -> Html Msg
createScriptView { scriptForm, script } =
    let
        mode =
            if scriptForm.id == "" then
                "作成"

            else
                "更新"

        title =
            "脚本" ++ mode

        isScriptInvalid =
            case script of
                Just _ ->
                    False

                Nothing ->
                    True
    in
    Script.registerForm title
        [ Form.field
            (scriptFormView scriptForm)
        , Form.field
            [ div [ class "control" ]
                [ button [ class "button is-primary", disabled isScriptInvalid, onClick UpdateScript ] [ text mode ]
                ]
            ]
        , Form.field
            [ div [ class "columns is-mobile" ]
                [ div [ class "column  is-4 is-offset-8 control" ]
                    [ button [ class "button is-danger", onClick OpenModalConfirmScriptDelete ] [ text "削除" ]
                    ]
                ]
            ]
        , Form.field
            [ a [ href "/rooper/script/" ] [ text "戻る" ]
            ]
        , OpenSheet.tragedySetView scriptForm.set
        ]


scriptFormView : Script.RegisterForm -> List (Html Msg)
scriptFormView scriptForm =
    [ Form.field
        [ label [ class "label has-text-white" ] [ text "脚本名" ]
        , Form.control
            [ input [ class "input", required True, value scriptForm.name, onChange ChangeScriptName ] []
            ]
        , Form.errors (Script.getNameError scriptForm)
        ]
    , Form.field
        [ label
            [ class "label has-text-white" ]
            [ text "使用セット" ]
        , Form.control
            [ div [ class "select" ]
                [ Script.selectTragedySet ChangeTragedySet scriptForm
                ]
            ]
        ]
    , Form.field
        [ label
            [ class "label has-text-white" ]
            [ text "ルールY" ]
        , Form.control
            [ div [ class "select" ]
                [ Script.mainPlots ChangeMainPlot scriptForm.mainPlot scriptForm
                ]
            ]
        ]
    , Form.field
        [ label
            [ class "label has-text-white" ]
            [ text "ルールX1" ]
        , Form.control
            [ div [ class "select" ]
                [ Script.subPlots1 ChangeSubPlot1 scriptForm.subPlot1 scriptForm
                ]
            ]
        ]

    -- FirstSteps用
    , if scriptForm.set.subPlotNumber == 2 then
        Form.field
            [ label
                [ class "label has-text-white" ]
                [ text "ルールX2" ]
            , Form.control
                [ div [ class "select" ]
                    [ Script.subPlots2 ChangeSubPlot2 scriptForm.subPlot2 scriptForm
                    ]
                ]
            ]

      else
        ExHtml.nothing
    , Form.field
        [ label
            [ class "label has-text-white" ]
            [ text "キャラクター" ]
        , button [ class "button is-info", onClick OpenCharacterSelectModal ] [ text "キャラクター選択" ]
        , Form.errors
            [ ( "キャラクターを追加してください", List.member Script.NoCharacterError (Script.errors scriptForm) )
            , ( "ルールで追加された役職を全て設定してください", List.member Script.InvalidCharacterRoles (Script.errors scriptForm) )
            ]
        ]
    , Form.field
        (characterFormCollection scriptForm)
    , Form.field
        [ label [ class "label has-text-white" ] [ text "ループ回数" ]
        , Form.control
            [ input [ class "input", Html.Attributes.min "1", type_ "number", required True, value <| String.fromInt scriptForm.numberOfLoops, onChange ChangeNumberOfLoops ] []
            ]
        ]
    , Form.field
        [ label [ class "label has-text-white" ] [ text "１ループ日数" ]
        , Form.control
            [ input [ class "input", Html.Attributes.min "1", type_ "number", required True, value <| String.fromInt scriptForm.daysInOneLoop, onChange ChangeDaysInOneLoop ] []
            ]
        ]
    , Form.field
        [ label
            [ class "label has-text-white" ]
            [ text "事件" ]
        , button [ class "button is-info", onClick OpenAddIncidentModal ] [ text "追加" ]
        ]
    , Form.field <| incidentCollection scriptForm
    , Form.field
        [ label [ class "label has-text-white" ] [ text "特別ルール" ]
        , Form.control
            [ textarea [ class "textarea", rows 8, value scriptForm.extra, onChange ChangeScriptExtra ] []
            ]
        ]
    , Form.field
        [ label [ class "label has-text-white" ] [ text "メモ" ]
        , Form.control
            [ textarea [ class "textarea", rows 8, value scriptForm.memo, onChange ChangeScriptMemo ] []
            ]
        ]
    ]


incidentCollection : Script.RegisterForm -> List (Html Msg)
incidentCollection scriptForm =
    scriptForm.incidents
        -- 日付順にソート
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
                    , div [ class "media-right ", style "align-self" "center" ]
                        [ div [ class "tag is-danger", onClick (DeleteIncidents data) ] [ text "削除", button [ class "delete is-small" ] [] ]
                        ]
                    ]
            )


characterFormCollection : Script.RegisterForm -> List (Html Msg)
characterFormCollection scriptForm =
    scriptForm.characters
        |> List.reverse
        -- 選んだ順に表示するため並び替え
        |> List.map
            (\c ->
                Character.characterFormCollectionItem OpenCharacterCardModal
                    c
                    [ Script.characterRoles c (ChangeCharacterRole c) scriptForm
                    , case c.character.characterType of
                        Character.TransferStudent ->
                            Form.field
                                [ label [ class "label has-text-white" ] [ text "登場日" ]
                                , input [ required True, type_ "number", value <| String.fromInt (Maybe.withDefault 0 c.optionalNumber), onChange (ChangeOptionalNumber c) ] []
                                ]

                        Character.GodlyBeing ->
                            Form.field
                                [ label [ class "label has-text-white" ] [ text "登場ループ" ]
                                , input [ required True, type_ "number", value <| String.fromInt (Maybe.withDefault 0 c.optionalNumber), onChange (ChangeOptionalNumber c) ] []
                                ]

                        Character.Boss ->
                            Form.field
                                [ label [ class "label has-text-white" ] [ text "テリトリー" ]
                                , Character.characterTurfBoards c (ChangeTurf c)
                                ]

                        _ ->
                            ExHtml.nothing
                    ]
            )
