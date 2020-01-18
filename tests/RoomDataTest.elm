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
        ]
