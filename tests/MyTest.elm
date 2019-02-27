module MyTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Blog exposing (isoTimeToDate, articleParser, Article)
import Json.Decode as D


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
                \_ -> D.decodeString
                        articleParser
                        "{\"title\":\"タイトル\",\"url\":\"http://hoge.com\",\"date\":\"1990-10-23T12:00:00+09:00\"}"
                      |> Expect.equal (Result.Ok <| Article "タイトル" "http://hoge.com" "1990/10/23")

            ]
        ]
