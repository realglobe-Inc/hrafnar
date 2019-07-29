module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe
import Page.Editor as Editor
import Page.Index as Index
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
    = Index Index.Model
    | Editor Editor.Model
    | NotFound


type Language
    = Japanese
    | English


type Msg
    = Link UrlRequest
    | UrlChanged Url
    | EditorMsg Editor.Msg
    | IndexMsg Index.Msg
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
                [ UP.map (stepIndex model <| Index.init) UP.top
                , UP.map (stepEditor model <| Editor.init) (UP.s "editor")
                ]
    in
    UP.parse parser url
        |> Maybe.withDefault ( { model | route = NotFound }, Cmd.none )


stepEditor : Model -> ( Editor.Model, Cmd Editor.Msg ) -> ( Model, Cmd Msg )
stepEditor model ( model_, cmd ) =
    ( { model | route = Editor model_ }, Cmd.map EditorMsg cmd )

stepIndex : Model -> ( Index.Model, Cmd Index.Msg ) -> ( Model, Cmd Msg )
stepIndex model ( model_, cmd ) =
    ( { model | route = Index model_ }, Cmd.map IndexMsg cmd )


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

        ( IndexMsg msg_, Index model_ ) ->
            stepIndex model <| Index.update model_ msg_

        _ ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Hrafnar"
    , body =
        [ div [ classList [ ("container", True)
                          , ("ravens-are-in-odins-service", True)
                          ]
              ]
              [ navigation model
              , case model.route of
                    Index model_ ->
                        Html.map IndexMsg <| Index.view model_

                    Editor model_ ->
                        Html.map EditorMsg <| Editor.view model_

                    NotFound ->
                        notFound model
              ]
        ]
    }

navigation : Model -> Html Msg
navigation model =
    div [ class "navigation" ]
        [ h1 [] [ text "HRAFNAR" ]
        , ul []
            [ li [] [ text "Dashboard" ]
            , li [] [ text "Project" ]
            , li [] [ text "Process" ]
            , li [] [ text "Settings" ]
            ]
        ]

notFound : Model -> Html Msg
notFound model =
    text "nyaan..."
