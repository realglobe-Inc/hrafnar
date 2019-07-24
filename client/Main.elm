module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe
import Page.Editor as Editor
import Url exposing (Url)
import Url.Parser as UP exposing ((</>), Parser)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = Link
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Key
    , route : Route
    , language : Language
    }


type Route
    = Index
    | Editor Editor.Model
    | NotFound


type Language
    = Japanese
    | English


type Msg
    = Link UrlRequest
    | UrlChanged Url
    | EditorMsg Editor.Msg
    | NoOp


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url k =
    stepUrl url
        { key = k
        , route = NotFound
        , language = Japanese
        }


stepUrl : Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        parser =
            UP.oneOf
                [ UP.map ( { model | route = Index }, Cmd.none ) UP.top
                , UP.map (stepEditor model <| Editor.init) (UP.s "editor")
                ]
    in
    UP.parse parser url
        |> Maybe.withDefault ( { model | route = NotFound }, Cmd.none )


stepEditor : Model -> ( Editor.Model, Cmd Editor.Msg ) -> ( Model, Cmd Msg )
stepEditor model ( model_, cmd ) =
    ( { model | route = Editor model_ }, Cmd.map EditorMsg cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.route ) of
        ( Link req, _ ) ->
            case req of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            stepUrl url model

        ( EditorMsg msg_, Editor model_ ) ->
            stepEditor model <| Editor.update model_ msg_

        _ ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Realglobe"
    , body =
        [ div [ class "container" ]
            [ case model.route of
                Index ->
                    index model

                Editor model_ ->
                    Html.map EditorMsg <| Editor.view model_

                NotFound ->
                    notFound model
            ]
        ]
    }


index : Model -> Html Msg
index model =
    div []
        [ h1 [] [ text "index" ]
        , a [ href "/editor" ] [ text "go to editor" ]
        ]


notFound : Model -> Html Msg
notFound model =
    text "nyaan..."
