module Page.Index exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Encode as JE
import Json.Decode as JD


type alias Model =
    { code : String
    , message : String
    }


type alias PostSourceResponse =
    { parse : Bool
    , typeCheck : Bool
    , message : Maybe String
    }


type Msg
    = TextInput String
    | OnSubmit
    | OnSubmitCompleted (Result Http.Error PostSourceResponse)


init : ( Model, Cmd Msg )
init =
    ( Model "" "", Cmd.none )


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        TextInput c ->
            ( { model | code = c }, Cmd.none )

        OnSubmit ->
            let
                body = JE.object
                       [ ( "source", JE.string model.code ) ]
                decoder = JD.map3 PostSourceResponse
                          (JD.at ["parse"] JD.bool)
                          (JD.at ["typeCheck"] JD.bool)
                          (JD.at ["message"] (JD.maybe JD.string))
            in
                ( model
                , Http.post
                    { url = "/api/source"
                    , body = Http.jsonBody body
                    , expect = Http.expectJson OnSubmitCompleted decoder
                    }
                )

        OnSubmitCompleted (Ok result) ->
            case result.message of
                Just m ->
                    ( { model | message = m }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )
        OnSubmitCompleted (Err _) ->
                    ( { model | message = "something error" }, Cmd.none )



view : Model -> Html Msg
view model = text  "index dayo"
