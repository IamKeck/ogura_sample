module Blog exposing (Article, Flag, Model, Msg(..), init, main, subscriptions, update, view)

import Browser exposing (element)
import Html exposing (Html, div, li, p, text, ul, a)
import Html.Attributes exposing (class, id, href)
import Xml.Decode as XD
import Http
import Debug
import Iso8601 exposing (toTime)
import Time


type alias Article =
    { title : String, url : String, date : String }


type alias Model =
    List Article


type alias Flag =
    ()


type Msg
    = GotArticles (Result Http.Error String)

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

articleParser : XD.Decoder (Article)
articleParser = XD.map3 Article
    (XD.path ["title"] (XD.single XD.string))
    (XD.path ["link"] (XD.index 0 <| XD.stringAttr "href"))
    (XD.path ["published"] ( XD.single <| XD.map isoTimeToDate XD.string))

articleListParser : XD.Decoder (List Article)
articleListParser = XD.path ["entry"] (XD.list articleParser)

getArticles : String -> Cmd Msg
getArticles url = Http.get {url= url, expect=Http.expectString GotArticles }

firstModel : List Article
firstModel =
    [ Article "テスト記事1" "http://www.google.com" "2018/10/1"
    , Article "テスト記事2" "http://www.google.com" "2018/10/2"
    , Article "テスト記事3" "http://www.google.com" "2018/10/3"
    , Article "テスト記事4" "http://www.google.com" "2018/10/4"
    ]

init : Flag -> ( Model, Cmd Msg )
init =
    always ( firstModel, getArticles "https://oguranaoya.hatenablog.com/feed")


view : Model -> Html Msg
view model =
    div [ id "news" ]
        [ p [ class "whats_new" ] [ text "What's New?" ]
        , ul [] <| List.map (\article ->
            li [ class "news_item" ]
                [ p [ class "date" ] [ text article.date ]
                , p [ class "article" ] [ a [ href article.url ] [text article.title] ]
                ]
            ) model
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    GotArticles r -> case r of
        Err e -> Debug.log (Debug.toString e) (model, Cmd.none)
        Ok xml -> case XD.run articleListParser (Debug.log "got xml" xml) of
            Err e -> Debug.log (Debug.toString e) (model, Cmd.none)
            Ok articles -> (articles, Cmd.none)



subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


main =
    element { init = init, view = view, update = update, subscriptions = subscriptions }
