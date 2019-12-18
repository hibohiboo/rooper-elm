module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Component.Form as Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onChange)
import Json.Encode exposing (Value)
import Models.Room as Room exposing (Room)
import Models.RoomName as RoomName exposing (RoomName)
import Models.User exposing (User)
import Ports exposing (..)
import Task exposing (Task)
import Views.RoomName as RoomNameView


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
    Model flags MenuClose Room.init Nothing Nothing



-- UPDATE


type Msg
    = Error String
    | OpenMenu
    | CloseMenu
    | SignOut
    | ChangeRoomName String
    | ChangeRoomId String
    | UpdateRoom
    | ReadRooms Value


type MenuState
    = MenuClose
    | MenuOpen


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

        ChangeRoomName name ->
            ( { model | roomForm = Room.setName name model.roomForm }, Cmd.none )

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

        ReadRooms val ->
            ( { model | rooms = RoomName.decodeRoomNameListFromJson val }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ readRooms ReadRooms ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ nav [ class "page-head" ]
            [ div [ class "left" ] [ text "惨劇  RoopeR online tool" ]
            , headNavRight model
            ]
        , main_ []
            [ div [ class "center box" ] [ mainMessage model ]
            ]
        ]


mainMessage : Model -> Html Msg
mainMessage model =
    case model.loginUser of
        Just user ->
            mainContent model

        Nothing ->
            loginMessage


mainContent : Model -> Html Msg
mainContent model =
    let
        { rooms } =
            model
    in
    case rooms of
        Just r ->
            RoomNameView.rooms r

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
