port module Blog exposing (Article, Flag, Model, Msg(..), articleListParser, articleParser, getArticles, getNextLive, gotParsedXml, init, isoTimeToDate, main, sendXml, subscriptions, update, view)

import Browser exposing (Document, document)
import Browser.Navigation exposing (load)
import Html exposing (Html, a, div, h1, h2, li, nav, p, pre, text, ul)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Http
import Iso8601 exposing (toTime)
import Json.Decode as D
import Json.Encode as E
import Time


type alias Article =
    { title : String, url : String, date : String }


type alias Model =
    { articles : List Article, waitingXml : Bool, nextLive : String }


type alias Flag =
    ()


type Msg
    = GotArticles (Result Http.Error String)
    | GotNextLive (Result Http.Error String)
    | MoveTo String
    | GotParsedXml E.Value
    | NextLiveClicked



-- TODO: UTC -> JST


isoTimeToDate : String -> String
isoTimeToDate s =
    let
        zone =
            Time.customZone (9 * 60) []
    in
    case toTime s of
        Err _ ->
            ""

        Ok time ->
            let
                y =
                    Time.toYear zone time |> String.fromInt

                m =
                    case Time.toMonth zone time of
                        Time.Jan ->
                            "1"

                        Time.Feb ->
                            "2"

                        Time.Mar ->
                            "3"

                        Time.Apr ->
                            "4"

                        Time.May ->
                            "5"

                        Time.Jun ->
                            "6"

                        Time.Jul ->
                            "7"

                        Time.Aug ->
                            "8"

                        Time.Sep ->
                            "9"

                        Time.Oct ->
                            "10"

                        Time.Nov ->
                            "11"

                        Time.Dec ->
                            "12"

                d =
                    Time.toDay zone time |> String.fromInt
            in
            y ++ "/" ++ (zeroPadding 2 m) ++ "/" ++ (zeroPadding 2 d)

zeroPadding : Int -> String -> String
zeroPadding i = String.append (String.repeat (i - 1) "0") >> String.right i

articleParser : D.Decoder Article
articleParser =
    D.map3 Article
        (D.field "title" D.string)
        (D.field "url" D.string)
        (D.field "date" D.string |> D.map isoTimeToDate)


articleListParser : D.Decoder (List Article)
articleListParser =
    D.list articleParser


getArticles : String -> Cmd Msg
getArticles url =
    Http.get { url = url, expect = Http.expectString GotArticles }


getNextLive : Cmd Msg
getNextLive =
    Http.get { url = "/next_live.txt", expect = Http.expectString GotNextLive }


port sendXml : String -> Cmd msg


port scrollToNextLive : () -> Cmd msg


port gotParsedXml : (E.Value -> msg) -> Sub msg


init : Flag -> ( Model, Cmd Msg )
init =
    always
        ( Model [] False ""
        , Cmd.batch
            [ getArticles "https://oguranaoya.hatenablog.com/feed"
            , getNextLive
            ]
        )


view : Model -> Document Msg
view model =
    let
        body =
            [ div [ id "background", class "page" ]
                [ nav [] [
                    div [id "global_menu_outer"] 
                    [ ul [ id "global_menu" ]
                        [ li [] [ text "HOME" ]
                        , li [] [ text "NEWS" ]
                        , li [] [ text "ABOUT" ]
                        , li [] [ text "CONTACT" ]
                        , li [] [ a [ href "https://oguranaoya.hatenablog.com/" ] [ text "BLOG" ] ]
                        , li [] [ text "DISCOGRAPHY" ]
                        ]
                    ]
                    ]
                , div [ id "title" ]
                    [ h2 [] [ text "JAZZ TRUMPET PLAYER" ]
                    , h1 [] [ text "NAOYA OGURA" ]
                    , div [ id "news" ]
                        [ p [ class "whats_new" ] [ text "What's New?" ]
                        , ul [] <|
                            List.map
                                (\article ->
                                    li [ class "news_item", onClick <| MoveTo article.url ]
                                        [ p [ class "date" ] [ text article.date ]
                                        , p [ class "article" ] [ text article.title ]
                                        ]
                                )
                                model.articles
                        ]
                    ]
                , a [ id "next_live", class "animated infinite bounce", onClick NextLiveClicked ] []
                ]
            , div [ id "second_page", class "page" ]
                [ div [ id "live_article" ]
                    [ pre [] [ text model.nextLive ] ]
                ]
            ]

        title =
            "Jazz Trumpet Player 小倉直也"
    in
    Document title body


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotArticles r ->
            case r of
                Err _ ->
                    ( model, Cmd.none )

                Ok xml ->
                    ( { model | waitingXml = True }, sendXml xml )

        GotNextLive r ->
            case r of
                Err _ ->
                    ( model, Cmd.none )

                Ok nextLive ->
                    ( { model | nextLive = nextLive }, Cmd.none )

        MoveTo url ->
            ( model, load url )

        GotParsedXml v ->
            case D.decodeValue articleListParser v of
                Err _ ->
                    ( { model | waitingXml = False }, Cmd.none )

                Ok articles ->
                    ( { model | articles = articles, waitingXml = False }, Cmd.none )

        NextLiveClicked ->
            ( model, scrollToNextLive () )


subscriptions : Model -> Sub Msg
subscriptions m =
    case m.waitingXml of
        True ->
            gotParsedXml GotParsedXml

        False ->
            Sub.none


main =
    document { init = init, view = view, update = update, subscriptions = subscriptions }
