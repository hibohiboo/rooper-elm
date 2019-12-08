port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (Value)
import Task exposing (Task)


port errorToJs : String -> Cmd msg


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { test : String
    }


init : Value -> ( Model, Cmd Msg )
init flags =
    ( Model "test new", Cmd.batch [] )



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
    div []
        [ text model.test
        ]
