module Page.Project exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as JE
import Json.Decode as JD

type alias Project =
    { name : String
    , date : Int
    }

type alias Model =
    { newProjectName : String
    , projects : List Project
    }


type Msg
    = GetProjectList
    | GetProjectListCompleted (Result Http.Error (List Project))
    | InputProjectName String
    | CreateProject
    | CreateProjectCompleted (Result Http.Error ())
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( Model "" [], Cmd.none )


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of

        GetProjectList ->
            ( model, Cmd.none )

        GetProjectListCompleted (Ok result) ->
            ( { model | projects = result } , Cmd.none )

        GetProjectListCompleted (Err _) ->
            ( model, Cmd.none )

        InputProjectName name ->
            ( { model | newProjectName = name }, Cmd.none )

        CreateProject ->
            ( { model | newProjectName = "" }
            , Http.post
                { url = "/api/project/" ++ model.newProjectName
                , body = Http.emptyBody 
                , expect = Http.expectJson CreateProjectCompleted (JD.succeed ())
                }
            )

        CreateProjectCompleted (Ok _) ->
            ( model, Cmd.none )

        CreateProjectCompleted (Err _) ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        toMsg k =
            case k of
                13 -> msg -- 13 means enter
                _ -> NoOp
    in
        on "keypress" (JD.map toMsg keyCode)

view : Model -> Html Msg
view model = main_ []
             [ input [ onInput InputProjectName
                     , onEnter CreateProject
                     , placeholder "new project name"
                     , value model.newProjectName
                     ]
                   []
             , button
                   [ onClick CreateProject
                   ]
                   [ text "create new project" ]
             ]
