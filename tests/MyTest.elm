module MyTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Blog exposing (isoTimeToDate)


suite : Test
suite =
    describe "Blog Test"
        [ describe "日付パーステスト"
            [ test "ゾーンに依存しない日付" <|
                \_ -> isoTimeToDate "1990-10-23T12:00:00+09:00" |> Expect.equal "1990/10/23"
            , test "ゾーンに依存する日付" <|
                \_ -> isoTimeToDate "1990-10-23T00:00:00+09:00" |> Expect.equal "1990/10/23"
            ]
        ]
