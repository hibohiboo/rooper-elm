module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Component.Bulma as Bulma
import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onChange)
import Json.Encode exposing (Value)
import Models.Room as Room exposing (Room)
import Models.RoomName as RoomName exposing (RoomName)
import Models.Script as Script exposing (Script)
import Models.ScriptName as ScriptName exposing (ScriptName)
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
    , scriptForm : Script.RegisterForm
    , script : Maybe Script
    , scripts : Maybe (List ScriptName)
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
    Model flags MenuClose Room.init Room.initRoom RoomName.initRoomNames Script.initForm Script.initScript ScriptName.initScriptNames MainTab CloseModalState ""



-- Msg


type Msg
    = Error String
    | OpenMenu
    | CloseMenu
    | SignOut
    | ChangeRoomName String
    | ChangeRoomId String
    | UpdateRoom
    | ReadedRooms Value
    | ChangedUrl String
    | ChangeUrl String
    | OpenModal String
    | CloseModal
    | ChangeScriptName String
    | UpdateScript
    | ReadedScriptNames Value
    | ReadedScript Value
    | OpenModalConfirmScriptDelete
    | DeleteScript
    | DeletedScript Bool
    | ChangeTragedySet String
    | ChangeMainPlot String
    | ChangedScript


type MenuState
    = MenuClose
    | MenuOpen


type MainAreaState
    = MainTab
    | ScriptTab
    | NothingTab
    | ScriptCreateTab


type ModalState
    = OpenModalState
    | CloseModalState
    | ConfirmModalState Msg



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
            ( { model | modalState = ConfirmModalState DeleteScript }, Cmd.none )

        ChangeRoomName name ->
            ( { model | roomForm = Room.setName name model.roomForm }, Cmd.none )

        ChangeUrl url ->
            ( model, changeUrl url )

        ChangedUrl url ->
            case Route.toRoute url of
                Route.Top ->
                    ( { model | mainAreaState = MainTab }, Cmd.none )

                Route.Script ->
                    ( { model | mainAreaState = ScriptTab }, readScriptNames () )

                Route.ScriptCreate ->
                    ( { model | mainAreaState = ScriptCreateTab, scriptForm = Script.initForm }, Cmd.none )

                Route.ScriptEdit s ->
                    ( { model | mainAreaState = ScriptCreateTab }, readScript s )

                Route.NotFound ->
                    update (OpenModal ("指定されたURLが見つかりません。\nご確認お願いします。\n" ++ url)) { model | mainAreaState = NothingTab }

        ChangeRoomId id ->
            ( { model | roomForm = Room.setId id model.roomForm }, Cmd.none )

        UpdateRoom ->
            let
                room =
                    Room.convert model.roomForm

                cmd =
                    case room of
                        Nothing ->
                            Cmd.none

                        Just r ->
                            updateRoom <| Room.encode r
            in
            ( { model | room = room }, cmd )

        ReadedRooms val ->
            ( { model | rooms = RoomName.decodeRoomNameListFromJson val }, Cmd.none )

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
            ( { model | scriptForm = Script.setTragedySet val model.scriptForm }, Cmd.none )

        ChangeMainPlot val ->
            ( { model | scriptForm = Script.setMainPlot val model.scriptForm }, Cmd.none )

        ChangedScript ->
            ( { model | script = Script.convert model.scriptForm }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ readedRooms ReadedRooms
        , changedUrl ChangedUrl
        , readedScriptNames ReadedScriptNames
        , readedScript ReadedScript
        , deletedScript DeletedScript
        ]


view : Model -> Html Msg
view model =
    div [ class "rooper-container" ]
        [ nav [ class "page-head" ]
            [ div [ class "left" ] [ text "惨劇RoopeR online tool" ]
            , headNavRight model
            ]
        , main_ [ class "rooper-main" ] [ mainContent model ]
        , modal model
        ]


mainContent : Model -> Html Msg
mainContent model =
    case model.loginUser of
        Nothing ->
            mainContentBox model

        Just _ ->
            case model.mainAreaState of
                ScriptTab ->
                    mainContentBox model

                ScriptCreateTab ->
                    div [ class "content" ] [ createScriptView model ]

                MainTab ->
                    mainContentBox model

                NothingTab ->
                    text ""


mainContentBox : Model -> Html Msg
mainContentBox model =
    div [ class "center box" ]
        [ mainTabs model
        , mainMessage model
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
                ConfirmModalState message ->
                    div []
                        [ div [ class "columns is-mobile" ]
                            [ text "削除します。よろしいですか？"
                            ]
                        , div [ class "columns is-mobile" ]
                            [ div [ class "column  is-4  is-offset-1 control" ]
                                [ button [ class "button is-danger", onClick message ] [ text "削除" ]
                                ]
                            , div [ class "column  is-4 is-offset-2 control" ]
                                [ button [ class "button is-info", onClick CloseModal ] [ text "キャンセル" ] ]
                            ]
                        ]

                _ ->
                    text modalMessage
            ]


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
            text ""

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
            text ""

        ScriptTab ->
            mainScriptContent model

        ScriptCreateTab ->
            createScriptView model


mainScriptContent : Model -> Html Msg
mainScriptContent model =
    let
        { scripts } =
            model
    in
    div []
        [ div [ class "columns is-mobile" ]
            [ div [ class "column is-5 is-offset-7" ]
                [ Form.createButton (ChangeUrl "/rooper/script/create/")
                ]
            ]
        , case scripts of
            Just [] ->
                text "脚本はまだ作られていません"

            Just s ->
                ScriptName.scripts s

            _ ->
                text ""
        ]


mainTopContent : Model -> Html Msg
mainTopContent model =
    let
        { rooms } =
            model
    in
    case rooms of
        Just r ->
            RoomName.rooms r

        Nothing ->
            text ""



-- createRoomView : Model -> Html Msg
-- createRoomView { roomForm } =
--     Room.registerForm
--         [ Form.field
--             [ label [ class "label has-text-white" ]
--                 [ text "ルーム名"
--                 ]
--             , Form.control
--                 [ input [ class "input", required True, onInput ChangeRoomName ] []
--                 ]
--             , Form.errors (Room.getNameError roomForm)
--             ]
--         , div [ class "control" ]
--             [ button [ class "button is-primary", onClick UpdateRoom ] [ text "作成" ]
--             ]
--         ]


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
                        [ li [ onClick SignOut ] [ text "サインアウト" ]
                        ]
                    ]
                , img [ src user.twitterProfileImageUrl ] []
                ]

        Nothing ->
            text ""


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
        ]


scriptFormView : Script.RegisterForm -> List (Html Msg)
scriptFormView scriptForm =
    [ Form.field
        [ label [ class "label has-text-white" ] [ text "脚本名" ]
        , Form.control
            [ input [ class "input", required True, value scriptForm.name, onInput ChangeScriptName ] []
            ]
        , Form.errors (Script.getNameError scriptForm)
        ]
    , Form.field
        [ label
            [ class "label has-text-white" ]
            [ text "使用セット" ]
        , Form.control
            [ div [ class "select" ]
                [ select [ onChange ChangeTragedySet ]
                    [ option [ value "FistSteps", selected (Script.isSetFirstSteps scriptForm) ] [ text "First Steps" ]
                    , option [ value "BasicTragedy", selected (Script.isSetBasicTragedy scriptForm) ] [ text "Basic Tragedy X" ]
                    ]
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
                [ Script.subPlots1 ChangeMainPlot scriptForm.subPlot1 scriptForm
                ]
            ]
        ]
    ]
