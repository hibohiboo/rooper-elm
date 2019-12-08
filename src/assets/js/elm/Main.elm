module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Models.User exposing (User)
import Ports exposing (..)
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
    Model flags MenuClose



-- UPDATE


type Msg
    = Error String
    | OpenMenu
    | CloseMenu
    | SignOut


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ nav [ class "page-head" ]
            [ div [ class "left" ] [ text "惨劇  RoopeR online tool" ]
            , headNavRight model
            ]
        , main_ []
            [ div [ class "center" ] [ mainMessage model ]
            ]
        ]


mainMessage : Model -> Html Msg
mainMessage model =
    case model.loginUser of
        Just user ->
            mainContent

        Nothing ->
            loginMessage


mainContent : Html Msg
mainContent =
    div [ class "swiper-container my-sw-container" ]
        [ div [ class "swiper-wrapper" ]
            [ div [ class "swiper-slide", attribute "data-history" "main" ] [ mainContentFirst ]
            , div [ class "swiper-slide", attribute "data-history" "create-room" ] [ createRoomView ]
            ]
        ]


mainContentFirst =
    div []
        [ p [ class "buttons" ]
            [ button [ class "button" ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-plus" ] []
                    ]
                , span [] [ text "新しいルームを作成" ]
                ]
            ]
        ]


createRoomView =
    div []
        [ h2 [] [ text "ルーム作成" ]
        , div []
            [ label []
                [ text "ルーム名"
                , input [] []
                ]
            ]
        ]


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
