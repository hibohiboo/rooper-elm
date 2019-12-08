port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Models.User as User exposing (User)
import Task exposing (Task)


port errorToJs : String -> Cmd msg


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
    }


init : Maybe User -> ( Model, Cmd Msg )
init flags =
    let
        _ =
            Debug.log "flags" flags
    in
    ( Model flags, Cmd.batch [] )



-- UPDATE


type Msg
    = Test


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Test ->
            ( model, "test" |> errorToJs )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ nav [ class "page-head" ]
            [ div [] [ text "Tragedy RoopeR online tool" ]
            , headNavRight model.loginUser
            ]
        , main_ [] []
        ]


headNavRight : Maybe User -> Html Msg
headNavRight maybeUser =
    case maybeUser of
        Just user ->
            div [ class "right" ] [ img [ src user.twitterProfileImageUrl ] [] ]

        Nothing ->
            text ""
