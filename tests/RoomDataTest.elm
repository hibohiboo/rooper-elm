module RoomDataTest exposing (unitTest)

import Expect
import Models.Character as Character
import Models.RoomData as RoomData
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
                    |> RoomData.resolveCards
                    |> List.length
                    |> Expect.equal 2
        ]
