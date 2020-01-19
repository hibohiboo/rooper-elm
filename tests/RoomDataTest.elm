module RoomDataTest exposing (unitTest)

import Expect
import Models.Board exposing (BoardType(..))
import Models.Character exposing (CharacterType(..))
import Models.RoomData as RoomData
import Models.RoomData.Board as RDBoard
import Models.RoomData.Character as RDCharacter
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
        , test "ボードに暗躍禁止を設置したときに暗躍が増えないこと" <|
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
        , test "ボードに主人公二人が暗躍禁止を設置したときに暗躍+を阻止できないこと" <|
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
        , test "キャラクターに暗躍+1を設置したときに暗躍が+１されること" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindHand 1 "m5"
                    |> RoomData.changeMasterMindComponent 1 "ShrineMaiden"
                    |> RoomData.resolveCards
                    |> .characters
                    |> RDCharacter.getCharacter ShrineMaiden
                    |> Maybe.map .intrigue
                    |> Maybe.map (\c -> Expect.equal 1 c)
                    |> Maybe.withDefault (Expect.fail "失敗")
        , test "キャラクターに暗躍+2を設置したときに暗躍が+2されること" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindHand 1 "m6"
                    |> RoomData.changeMasterMindComponent 1 "ShrineMaiden"
                    |> RoomData.resolveCards
                    |> .characters
                    |> RDCharacter.getCharacter ShrineMaiden
                    |> Maybe.map .intrigue
                    |> Maybe.map (\c -> Expect.equal 2 c)
                    |> Maybe.withDefault (Expect.fail "失敗")
        , test "キャラクターに暗躍禁止を設置したときに暗躍が増えないこと" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindHand 1 "m6"
                    |> RoomData.changeMasterMindComponent 1 "ShrineMaiden"
                    |> RoomData.changeProtagonistHand 1 "p4"
                    |> RoomData.changeProtagonistComponent 1 "ShrineMaiden"
                    |> RoomData.resolveCards
                    |> .characters
                    |> RDCharacter.getCharacter ShrineMaiden
                    |> Maybe.map .intrigue
                    |> Maybe.map (\c -> Expect.equal 0 c)
                    |> Maybe.withDefault (Expect.fail "失敗")
        , test "キャラクターに主人公二人が暗躍禁止を設置したときに暗躍+を阻止できないこと" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindHand 1 "m6"
                    |> RoomData.changeMasterMindComponent 1 "ShrineMaiden"
                    |> RoomData.changeProtagonistHand 1 "p4"
                    |> RoomData.changeProtagonistComponent 1 "City"
                    |> RoomData.changeProtagonistHand 2 "p4"
                    |> RoomData.changeProtagonistComponent 2 "ShrineMaiden"
                    |> RoomData.resolveCards
                    |> .characters
                    |> RDCharacter.getCharacter ShrineMaiden
                    |> Maybe.map .intrigue
                    |> Maybe.map (\c -> Expect.equal 2 c)
                    |> Maybe.withDefault (Expect.fail "失敗")
        , test "キャラクターに不安+1を設置したときに不安が+１されること" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindHand 1 "m0"
                    |> RoomData.changeMasterMindComponent 1 "ShrineMaiden"
                    |> RoomData.resolveCards
                    |> .characters
                    |> RDCharacter.getCharacter ShrineMaiden
                    |> Maybe.map .paranoia
                    |> Maybe.map (\c -> Expect.equal 1 c)
                    |> Maybe.withDefault (Expect.fail "失敗")
        , test "不安0のキャラクターに不安-1を設置したときに不安0となること" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindHand 1 "m2"
                    |> RoomData.changeMasterMindComponent 1 "ShrineMaiden"
                    |> RoomData.resolveCards
                    |> .characters
                    |> RDCharacter.getCharacter ShrineMaiden
                    |> Maybe.map .paranoia
                    |> Maybe.map (\c -> Expect.equal 0 c)
                    |> Maybe.withDefault (Expect.fail "失敗")
        , test "キャラクターに不安+1を2枚設置したときに不安が+2されること" <|
            \() ->
                RoomData.initDefault
                    |> RoomData.changeMasterMindHand 1 "m0"
                    |> RoomData.changeMasterMindComponent 1 "ShrineMaiden"
                    |> RoomData.changeProtagonistHand 2 "p0"
                    |> RoomData.changeProtagonistComponent 2 "ShrineMaiden"
                    |> RoomData.resolveCards
                    |> .characters
                    |> RDCharacter.getCharacter ShrineMaiden
                    |> Maybe.map .paranoia
                    |> Maybe.map (\c -> Expect.equal 2 c)
                    |> Maybe.withDefault (Expect.fail "失敗")
        ]
