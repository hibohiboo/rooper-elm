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
        , test "場に出したカード一覧が2枚のときにそれが取得できること" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindComponent 1 "ShrineMaiden"
                    |> RoomData.changeProtagonistComponent 1 "ShrineMaiden"
                    |> RoomData.playedHands
                    |> List.length
                    |> Expect.equal 2
        , test "ボードに暗躍+1を設置したときに暗躍が+１されること" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindHand 1 "m5"
                    |> RoomData.changeMasterMindComponent 1 "City"
                    |> RoomData.resolveCards
                    |> .boards
                    |> RDBoard.getBoard City
                    |> .intrigue
                    |> Expect.equal 1
        , test "ボードに暗躍+2を設置したときに暗躍が+2されること" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindHand 1 "m6"
                    |> RoomData.changeMasterMindComponent 1 "City"
                    |> RoomData.resolveCards
                    |> .boards
                    |> RDBoard.getBoard City
                    |> .intrigue
                    |> Expect.equal 2
        , test "暗躍禁止を設置したときに暗躍が増えないこと" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindHand 1 "m6"
                    |> RoomData.changeMasterMindComponent 1 "City"
                    |> RoomData.changeProtagonistHand 1 "p4"
                    |> RoomData.changeProtagonistComponent 1 "City"
                    |> RoomData.resolveCards
                    |> .boards
                    |> RDBoard.getBoard City
                    |> .intrigue
                    |> Expect.equal 0
        , test "主人公二人が暗躍禁止を設置したときに暗躍+を阻止できないこと" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindHand 1 "m6"
                    |> RoomData.changeMasterMindComponent 1 "City"
                    |> RoomData.changeProtagonistHand 1 "p4"
                    |> RoomData.changeProtagonistComponent 1 "City"
                    |> RoomData.changeProtagonistHand 2 "p4"
                    |> RoomData.changeProtagonistComponent 2 "ShrineMaiden"
                    |> RoomData.resolveCards
                    |> .boards
                    |> RDBoard.getBoard City
                    |> .intrigue
                    |> Expect.equal 2
        ]
