module Blog exposing (Article, Flag, Model, Msg(..), init, main, subscriptions, update, view)

import Browser exposing (element)
import Html exposing (Html, div, li, p, text, ul)
import Html.Attributes exposing (class, id)


type alias Article =
    { title : String, url : String, date : String }


type alias Model =
    List Article


type alias Flag =
    ()


type Msg
    = Noop

firstModel : List Article
firstModel =
    [ Article "テスト記事1" "http://www.google.com" "2018/10/1"
    , Article "テスト記事2" "http://www.google.com" "2018/10/2"
    , Article "テスト記事3" "http://www.google.com" "2018/10/3"
    , Article "テスト記事4" "http://www.google.com" "2018/10/4"
    ]

init : Flag -> ( Model, Cmd Msg )
init =
    always ( firstModel, Cmd.none )


view : Model -> Html Msg
view model =
    div [ id "news" ]
        [ p [ class "whats_new" ] [ text "What's New?" ]
        , ul [] <| List.map (\article ->
            li [ class "news_item" ]
                [ p [ class "date" ] [ text article.date ]
                , p [ class "article" ] [ text article.title ]
                ]
            ) model
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


main =
    element { init = init, view = view, update = update, subscriptions = subscriptions }
