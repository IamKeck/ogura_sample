port module Blog exposing (Article, Flag, Model, Msg(..), init, main, subscriptions, update, view)

import Browser exposing (element)
import Browser.Navigation exposing (load)
import Html exposing (Html, div, li, p, text, ul)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Xml.Decode as XD
import Http
import Iso8601 exposing (toTime)
import Time
import Json.Encode as E
import Json.Decode as D


type alias Article =
    { title : String, url : String, date : String }


type alias Model =
    {articles : List Article, waitingXml : Bool }


type alias Flag =
    ()


type Msg
    = GotArticles (Result Http.Error String)
    | MoveTo String
    | GotParsedXml E.Value

-- TODO: UTC -> JST
isoTimeToDate : String -> String
isoTimeToDate s = case toTime s of
    Err _ -> ""
    Ok time ->
        let
            y = Time.toYear Time.utc time |> String.fromInt
            m = case Time.toMonth Time.utc time of
                    Time.Jan -> "1"
                    Time.Feb -> "2"
                    Time.Mar -> "3"
                    Time.Apr -> "4"
                    Time.May -> "5"
                    Time.Jun -> "6"
                    Time.Jul -> "7"
                    Time.Aug -> "8"
                    Time.Sep -> "9"
                    Time.Oct -> "10"
                    Time.Nov -> "11"
                    Time.Dec -> "12"
            d = Time.toDay Time.utc time |> String.fromInt
        in
            y ++ "/" ++ m ++ "/" ++ d

articleParser : D.Decoder (Article)
articleParser = D.map3 Article
    (D.field "title" D.string)
    (D.field "url" D.string)
    (D.field "date" D.string |> D.map isoTimeToDate)

articleListParser : D.Decoder (List Article)
articleListParser = D.list articleParser

getArticles : String -> Cmd Msg
getArticles url = Http.get {url= url, expect=Http.expectString GotArticles }

port sendXml : String -> Cmd msg
port gotParsedXml : (E.Value ->  msg) -> Sub msg


init : Flag -> ( Model, Cmd Msg )
init =
    always ( Model [] False, getArticles "https://oguranaoya.hatenablog.com/feed")


view : Model -> Html Msg
view model =
    div [ id "news" ]
        [ p [ class "whats_new" ] [ text "What's New?" ]
        , ul [] <| List.map (\article ->
            li [ class "news_item", onClick <| MoveTo article.url ]
                [ p [ class "date" ] [ text article.date ]
                , p [ class "article" ] [ text article.title ]
                ]
            ) model.articles
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    GotArticles r -> case r of
        Err _ -> (model, Cmd.none)
        Ok xml -> ({model|waitingXml=True}, sendXml xml)
    MoveTo url -> (model, load url)
    GotParsedXml v -> case D.decodeValue articleListParser v of
        Err e -> Debug.log (D.errorToString e) ({model|waitingXml = False}, Cmd.none)
        Ok articles -> (Model articles False, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions m =
    case m.waitingXml of
        True -> gotParsedXml GotParsedXml
        False -> Sub.none


main =
    element { init = init, view = view, update = update, subscriptions = subscriptions }
