module MastermindTest exposing (unitTest)

import Expect
import Models.Character as Character
import Models.RoomData.MasterMind as MasterMind
import Test exposing (..)



----


unitTest : Test
unitTest =
    describe "脚本家"
        [ test "名前が脚本家であること" <|
            \() ->
                MasterMind.init "twitterName"
                    |> .name
                    |> Expect.equal "脚本家"
        , test "場に出したカード一覧を取得" <|
            \() ->
                MasterMind.init "twitterName"
                    |> MasterMind.changeMasterMindComponent 1 "ShrineMaiden"
                    |> MasterMind.getPlayedHands
                    |> List.length
                    |> Expect.equal 1
        ]
