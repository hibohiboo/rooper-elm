module RoomDataTest exposing (unitTest)

import Expect
import Models.Board exposing (BoardType(..))
import Models.Character as Character
import Models.RoomData as RoomData
import Models.RoomData.Board as RDBoard
import Test exposing (..)



----


unitTest : Test
unitTest =
    describe "ルームデータ"
        [ test "テスト用初期化確認" <|
            \() ->
                RoomData.initDefault
                    |> .id
                    |> Expect.equal "roomTest"
        , test "場に出したカード一覧を取得" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindComponent 1 "ShrineMaiden"
                    |> RoomData.changeProtagonistComponent 1 "ShrineMaiden"
                    |> RoomData.playedHands
                    |> List.length
                    |> Expect.equal 2
        , test "ボードに暗躍+1を設置" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindHand 1 "m5"
                    |> RoomData.changeMasterMindComponent 1 "City"
                    |> RoomData.resolveCards
                    |> .boards
                    |> RDBoard.getBoard City
                    |> .intrigue
                    |> Expect.equal 1
        , test "ボードに暗躍+2を設置" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindHand 1 "m6"
                    |> RoomData.changeMasterMindComponent 1 "City"
                    |> RoomData.resolveCards
                    |> .boards
                    |> RDBoard.getBoard City
                    |> .intrigue
                    |> Expect.equal 2
        ]
