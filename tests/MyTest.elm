module MyTest exposing (suite)

import Blog exposing (Article, Environment(..), articleParser, determineEnvFromDomain, isoTimeToDate)
import Expect exposing (Expectation)
import Json.Decode as D
import Test exposing (..)


suite : Test
suite =
    describe "Blog Test"
        [ describe "日付パーステスト"
            [ test "ゾーンに依存しない日付" <|
                \_ -> isoTimeToDate "1990-10-23T12:00:00+09:00" |> Expect.equal "1990/10/23"
            , test "ゾーンに依存する日付" <|
                \_ -> isoTimeToDate "1990-10-23T00:00:00+09:00" |> Expect.equal "1990/10/23"
            ]
        , describe "articleParserのテスト"
            [ test "正常" <|
                \_ ->
                    D.decodeString
                        articleParser
                        "{\"title\":\"タイトル\",\"url\":\"http://hoge.com\",\"date\":\"1990-10-23T12:00:00+09:00\"}"
                        |> Expect.equal (Result.Ok <| Article "タイトル" "http://hoge.com" "1990/10/23")
            ]
        , describe "ドメインから環境を判定するテスト"
            [ test "ローカル" <|
                \_ ->
                    determineEnvFromDomain "127.0.0.1"
                        |> Expect.equal Local
            , test "ステージング" <|
                \_ ->
                    determineEnvFromDomain "192.168.1.1"
                        |> Expect.equal Staging
            , test "本番" <|
                \_ ->
                    determineEnvFromDomain "some.domain.com"
                        |> Expect.equal Production
            ]
        ]
