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
import Models.Scenario as Scenario exposing (Scenario)
import Models.ScenarioName as ScenarioName exposing (ScenarioName)
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
    , scenarioForm : Scenario.RegisterForm
    , scenario : Maybe Scenario
    , scenarios : Maybe (List ScenarioName)
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
    Model flags MenuClose Room.init Room.initRoom RoomName.initRoomNames Scenario.initForm Scenario.initScenario ScenarioName.initScenarioNames MainTab CloseModalState ""



-- UPDATE


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
    | ChangeScenarioName String
    | UpdateScenario
    | ReadedScenarioNames Value
    | ReadedScenario Value


type MenuState
    = MenuClose
    | MenuOpen


type MainAreaState
    = MainTab
    | ScenarioTab
    | NothingTab
    | ScenarioCreateTab


type ModalState
    = OpenModalState
    | CloseModalState


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

        ChangeRoomName name ->
            ( { model | roomForm = Room.setName name model.roomForm }, Cmd.none )

        ChangeUrl url ->
            ( model, changeUrl url )

        ChangedUrl url ->
            case Route.toRoute url of
                Route.Top ->
                    ( { model | mainAreaState = MainTab }, Cmd.none )

                Route.Scenario ->
                    ( { model | mainAreaState = ScenarioTab }, readScenarioNames () )

                Route.ScenarioCreate ->
                    ( { model | mainAreaState = ScenarioCreateTab, scenarioForm = Scenario.initForm }, Cmd.none )

                Route.ScenarioEdit s ->
                    ( { model | mainAreaState = ScenarioCreateTab }, readScenario s )

                Route.NotFound ->
                    update (OpenModal "指定されたURLが見つかりません。\nご確認お願いします。") { model | mainAreaState = NothingTab }

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

        ChangeScenarioName name ->
            ( { model | scenarioForm = Scenario.setName name model.scenarioForm }, Cmd.none )

        ReadedScenarioNames val ->
            ( { model | scenarios = ScenarioName.decodeScenarioNameListFromJson val }, Cmd.none )

        UpdateScenario ->
            let
                scenario =
                    Scenario.convert model.scenarioForm
            in
            case scenario of
                Nothing ->
                    update (OpenModal "保存に失敗しました。項目を再確認してください") { model | scenario = scenario }

                Just s ->
                    ( { model | scenario = scenario, scenarioForm = Scenario.initForm }, updateScenario <| Scenario.encode s )

        ReadedScenario val ->
            let
                registerForm =
                    Scenario.decodeScenarioRegisterFormFromJson val
            in
            case registerForm of
                Just f ->
                    ( { model | scenarioForm = f }, Cmd.none )

                Nothing ->
                    update (OpenModal "読み込みに失敗しました。一度トップに戻ります。") { model | mainAreaState = MainTab }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ readedRooms ReadedRooms, changedUrl ChangedUrl, readedScenarioNames ReadedScenarioNames, readedScenario ReadedScenario ]


view : Model -> Html Msg
view model =
    div [ class "rooper-container" ]
        [ nav [ class "page-head" ]
            [ div [ class "left" ] [ text "惨劇RoopeR online tool" ]
            , headNavRight model
            ]
        , main_ [ class "rooper-main" ]
            [ div [ class "center box" ]
                [ mainTabs model
                , mainMessage model
                ]
            ]
        , modal model
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
        div [ class "box rooper-modal-message" ] [ text modalMessage ]


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

        scenarioTabClass =
            case mainAreaState of
                ScenarioTab ->
                    class "is-active"

                ScenarioCreateTab ->
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
                    [ li [ mainTabClass ] [ a [ href "/rooper" ] [ text "メイン" ] ]
                    , li [ scenarioTabClass ] [ a [ href "/rooper/scenario" ] [ text "シナリオ" ] ]
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

        ScenarioTab ->
            mainScenarioContent model

        ScenarioCreateTab ->
            createScenarioView model


mainScenarioContent : Model -> Html Msg
mainScenarioContent model =
    let
        { scenarios } =
            model
    in
    div []
        [ div [ class "columns is-mobile" ]
            [ div [ class "column is-5 is-offset-7" ]
                [ Form.createButton (ChangeUrl "/rooper/scenario/create")
                ]
            ]
        , case scenarios of
            Just [] ->
                text "シナリオはまだ作られていません"

            Just s ->
                ScenarioName.scenarios s

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


createScenarioView : Model -> Html Msg
createScenarioView { scenarioForm } =
    let
        mode =
            if scenarioForm.id == "" then
                "作成"

            else
                "更新"

        title =
            "シナリオ" ++ mode
    in
    Scenario.registerForm title
        [ Form.field
            [ label [ class "label has-text-white" ]
                [ text "シナリオ名"
                ]
            , Form.control
                [ input [ class "input", required True, value scenarioForm.name, onInput ChangeScenarioName ] []
                ]
            , Form.errors (Scenario.getNameError scenarioForm)
            ]
        , div [ class "control" ]
            [ button [ class "button is-primary", onClick UpdateScenario ] [ text mode ]
            ]
        ]
