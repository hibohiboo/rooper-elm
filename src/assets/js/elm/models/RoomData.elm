module Models.RoomData exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import Json.Encode.Extra as ExEncode
import Models.Room as Room exposing (Room)
import Models.RoomData.MasterMind as MasterMind exposing (MasterMind)
import Models.RoomData.OpenSheet as OpenSheet exposing (OpenSheet)
import Models.RoomData.Protagonist as Protagonist exposing (Protagonist)
import Models.Script as Script exposing (Script)
import Models.User exposing (User)


type alias RoomData =
    { id : String
    , mastermind : MasterMind
    , protagonists : List Protagonist
    , openSheet : OpenSheet
    , script : Maybe Script
    , loop : Int
    , date : Int
    }


init : Maybe RoomData
init =
    Nothing


initRoomData : Room -> RoomData
initRoomData room =
    let
        mastermind =
            MasterMind.init room.mastermindTwitterScreenName

        protagonists =
            Protagonist.init room.protagonist1TwitterScreenName room.protagonist2TwitterScreenName room.protagonist3TwitterScreenName
    in
    RoomData (Room.getId room) mastermind protagonists (Script.scriptToOpenSheet room.script) Nothing 1 1



-- Method


isRoomOwner : Maybe RoomData -> Bool
isRoomOwner room =
    case room of
        Just _ ->
            True

        Nothing ->
            False


isRoomMember : RoomData -> User -> Bool
isRoomMember data user =
    Protagonist.isProtagonist user.twitterScreenName data.protagonists



-- ==============================================================================================
-- setter
-- ==============================================================================================


setScript : Maybe Script -> RoomData -> RoomData
setScript s f =
    { f | script = s }



-- ==============================================================================================
-- デコーダ
-- ==============================================================================================


decode : D.Value -> Maybe RoomData
decode json =
    D.decodeValue decoder json
        |> Result.toMaybe


decoder : D.Decoder RoomData
decoder =
    D.succeed RoomData
        |> Pipeline.required "id" D.string
        |> Pipeline.required "mastermind" MasterMind.decoder
        |> Pipeline.required "protagonists" (D.list Protagonist.decoder)
        |> Pipeline.required "openSheet" OpenSheet.decoder
        |> Pipeline.optional "script" Script.scriptDecoder Nothing
        |> Pipeline.optional "loop" D.int 0
        |> Pipeline.optional "date" D.int 0



-- ==============================================================================================
-- エンコーダ
-- ==============================================================================================


encode : RoomData -> E.Value
encode data =
    E.object
        [ ( "id", E.string data.id )
        , ( "mastermind", MasterMind.encode data.mastermind )
        , ( "protagonists", E.list Protagonist.encode data.protagonists )
        , ( "openSheet", OpenSheet.encode data.openSheet )
        , ( "script", ExEncode.maybe Script.encode data.script )
        , ( "loop", E.int data.loop )
        , ( "date", E.int data.date )
        ]



-- ==============================================================================================
-- View
-- ==============================================================================================


openSheetView : RoomData -> Html msg
openSheetView data =
    data.openSheet
        |> OpenSheet.openSheetView


closeSheetView : RoomData -> Html msg
closeSheetView data =
    case data.script of
        Just script ->
            Script.closeSheet script

        Nothing ->
            text ""


tags : Maybe RoomData -> User -> Html msg
tags data user =
    case data of
        Nothing ->
            text ""

        Just d ->
            div []
                (List.concat
                    [ [ if MasterMind.isMasterMind user.twitterScreenName d.mastermind then
                            span [ class "tag is-danger" ] [ text "脚本家" ]

                        else
                            text ""
                      ]
                    , d.protagonists
                        |> Protagonist.getUserProtagonists user.twitterScreenName
                        |> List.map
                            (\p ->
                                span
                                    [ class <|
                                        if p.number == 1 then
                                            "tag is-info"

                                        else if p.number == 2 then
                                            "tag is-warning"

                                        else
                                            "tag is-success"
                                    ]
                                    [ text p.name ]
                            )
                    ]
                )


infos : RoomData -> Html msg
infos data =
    let
        { loop, date, openSheet } =
            data
    in
    section [ class "section" ]
        [ table [ class "table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Loop" ]
                    , th [] [ text "Date" ]
                    , th [] [ text "事件" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [] [ text <| String.fromInt loop ]
                    , td [] [ text <| String.fromInt date ]
                    , td [] [ OpenSheet.incidentIcon date openSheet.incidents ]
                    ]
                ]
            ]
        ]
