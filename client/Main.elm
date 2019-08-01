module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe
import Page.Index as Index
import Page.Project as Project
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
    | Project Project.Model
    | NotFound


type Language
    = Japanese
    | English


type Msg
    = Link UrlRequest
    | UrlChanged Url
    | IndexMsg Index.Msg
    | ProjectMsg Project.Msg
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
                , UP.map (stepProject model <| Project.init) (UP.s "project")
                ]
    in
    UP.parse parser url
        |> Maybe.withDefault ( { model | route = NotFound }, Cmd.none )



stepIndex : Model -> ( Index.Model, Cmd Index.Msg ) -> ( Model, Cmd Msg )
stepIndex model ( model_, cmd ) =
    ( { model | route = Index model_ }, Cmd.map IndexMsg cmd )

stepProject : Model -> ( Project.Model, Cmd Project.Msg ) -> ( Model, Cmd Msg )
stepProject model ( model_, cmd ) =
    ( { model | route = Project model_ }, Cmd.map ProjectMsg cmd )

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

        ( IndexMsg msg_, Index model_ ) ->
            stepIndex model <| Index.update model_ msg_

        ( ProjectMsg msg_, Project model_ ) ->
            stepProject model <| Project.update model_ msg_

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
              , div [ class "content" ]
                    <| case model.route of
                        Index model_ ->
                            [ h1 [] [ text "Dashboard" ]
                            , Html.map IndexMsg <| Index.view model_
                            ]
                        Project model_ ->
                            [ h1 [] [ text "Project" ]
                            , Html.map ProjectMsg <| Project.view model_
                            ]
                        NotFound ->
                            [ notFound model ]
              ]
        ]
    }

navigation : Model -> Html Msg
navigation model =
    div [ class "navigation" ]
        [ h1 [] [ text "HRAFNAR" ]
        , ul []
            [ li [] [ a [ href "/" ] [ text "Dashboard" ] ]
            , li [] [ a [ href "/project" ] [ text "Project" ] ]
            , li [] [ a [ href "/process" ] [ text "Process" ] ]
            , li [] [ a [ href "/settings" ] [ text "Settings" ] ]
            ]
        ]

notFound : Model -> Html Msg
notFound model =
    text "nyaan..."
