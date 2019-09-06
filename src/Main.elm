module Main exposing (Model, init, main)

import Browser
import Html exposing (Html, div, text)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = always Sub.none }



-- MODEL


type alias Model =
    { name : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "Gagas", Cmd.none )


type Msg
    = Hamik


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hamik ->
            ( { model | name = "Xajhak" }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [] [ text model.name ]
