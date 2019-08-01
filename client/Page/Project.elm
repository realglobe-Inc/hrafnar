module Page.Project exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Http
import Json.Encode as JE
import Json.Decode as JD

type alias Project =
    { name : String
    , date : Int
    }

type alias Model =
    { projects : List Project
    }


type Msg
    = GetProjectList
    | GetProjectListCompleted (Result Http.Error (List Project))


init : ( Model, Cmd Msg )
init =
    ( Model [], Cmd.none )


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of

        GetProjectList ->
            ( model, Cmd.none )
        GetProjectListCompleted (Ok result) ->
            ( { model | projects = result } , Cmd.none )

        GetProjectListCompleted (Err _) ->
            ( model, Cmd.none )



view : Model -> Html Msg
view model = text "project dayo"
