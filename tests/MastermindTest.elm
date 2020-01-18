module MastermindTest exposing (unitTest)

import Expect
import Models.RoomData.MasterMind as MasterMind
import Test exposing (..)
import Models.Character as Character


----


unitTest : Test
unitTest =
    describe "脚本家"
        [ test "名前が脚本家であること" <|
            \() ->
                MasterMind.init "twitterName"
                    |> .name
                    |> Expect.equal "脚本家"
       , test "選択中のカード一覧を取得" <|
            \() ->
                MasterMind.init "twitterName"
                    |> MasterMind.changeMasterMindComponent 1 "ShrineMaiden"
                    |> MasterMind.getSelectedHands
                    |> List.length
                    |> Expect.equal 1
        ]
