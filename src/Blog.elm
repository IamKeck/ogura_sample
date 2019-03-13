port module Blog exposing (Article, Environment(..), Flag, Model, Msg(..), articleListParser, articleParser, determineEnvFromDomain, getArticles, gotParsedXml, init, isoTimeToDate, main, sendXml, subscriptions, update, view)

import Browser exposing (Document, document)
import Browser.Navigation exposing (load)
import Html exposing (Html, a, div, h1, h2, i, li, nav, p, pre, text, ul)
import Html.Attributes exposing (class, classList, href, id)
import Html.Events exposing (onClick)
import Http
import Iso8601 exposing (toTime)
import Json.Decode as D
import Json.Encode as E
import Time


type alias Article =
    { title : String, url : String, date : String }


type alias Model =
    { articles : List Article
    , waitingXml : Bool
    , nextLive : String
    , showSpMenu : Bool
    , env : Environment
    }


type alias FirebaseData =
    { staging : String
    , production : String
    }


type alias Flag =
    String


type Msg
    = GotArticles (Result Http.Error String)
    | GotNextLive (Result Http.Error String)
    | MoveTo String
    | GotParsedXml E.Value
    | NextLiveClicked
    | ToggleSpMenu
    | DbUpdated FirebaseData


type Environment
    = Local
    | Staging
    | Production


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
            y ++ "/" ++ zeroPadding 2 m ++ "/" ++ zeroPadding 2 d


determineEnvFromDomain : String -> Environment
determineEnvFromDomain d =
    if d == "127.0.0.1" then
        Local

    else if String.toList d |> List.all (predOr (\a -> a == '.') Char.isDigit) then
        Staging

    else
        Production


predOr : (a -> Bool) -> (a -> Bool) -> (a -> Bool)
predOr fa fb =
    \a -> fa a || fb a


zeroPadding : Int -> String -> String
zeroPadding i =
    String.append (String.repeat (i - 1) "0") >> String.right i


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


port sendXml : String -> Cmd msg


port scrollToNextLive : () -> Cmd msg


port gotParsedXml : (E.Value -> msg) -> Sub msg


port openUrlInNewWindow : String -> Cmd msg


port dbUpdated : (FirebaseData -> msg) -> Sub msg


init : Flag -> ( Model, Cmd Msg )
init domain =
    ( Model [] False "" False <| determineEnvFromDomain domain
    , Cmd.batch
        [ getArticles "https://oguranaoya.hatenablog.com/feed"
        ]
    )


view : Model -> Document Msg
view model =
    let
        body =
            [ div [ id "background", class "page" ]
                [ nav []
                    [ div [ id "global_menu_outer" ]
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
                , nav []
                    [ div [ id "global_menu_sp_toggle", onClick ToggleSpMenu ]
                        [ i [ class "fas fa-bars" ] [] ]
                    , ul
                        [ classList
                            [ ( "global_menu_sp_show", model.showSpMenu )
                            , ( "global_menu_sp", True )
                            ]
                        ]
                        [ li [] [ text "HOME" ]
                        , li [] [ text "NEWS" ]
                        , li [] [ text "ABOUT" ]
                        , li [] [ text "CONTACT" ]
                        , li [] [ a [ href "https://oguranaoya.hatenablog.com/" ] [ text "BLOG" ] ]
                        , li [] [ text "DISCOGRAPHY" ]
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
            ( model, openUrlInNewWindow url )

        GotParsedXml v ->
            case D.decodeValue articleListParser v of
                Err _ ->
                    ( { model | waitingXml = False }, Cmd.none )

                Ok articles ->
                    ( { model | articles = articles, waitingXml = False }, Cmd.none )

        NextLiveClicked ->
            ( model, scrollToNextLive () )

        ToggleSpMenu ->
            ( { model | showSpMenu = not model.showSpMenu }, Cmd.none )

        DbUpdated dbData ->
            let
                newNextLive =
                    case model.env of
                        Production ->
                            dbData.production

                        _ ->
                            dbData.staging
            in
            ( { model | nextLive = newNextLive }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions m =
    case m.waitingXml of
        True ->
            Sub.batch [ dbUpdated DbUpdated, gotParsedXml GotParsedXml ]

        False ->
            dbUpdated DbUpdated


main =
    document { init = init, view = view, update = update, subscriptions = subscriptions }
