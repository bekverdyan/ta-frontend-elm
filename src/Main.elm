module Main exposing (Model, init, main)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = always Sub.none }



-- MODEL


type alias Model =
    { username : String
    , password : String
    , submited : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" False
    , Cmd.none
    )


type Msg
    = Username String
    | Password String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Username username ->
            ( { model | username = username }
            , Cmd.none
            )

        Password password ->
            ( { model | password = password }
            , Cmd.none
            )

        Submit ->
            ( { model | submited = True }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row [ Row.centerMd, Row.middleXs ]
            [ Grid.col
                [ Col.sm4 ]
                [ h3 [] [ text "Back Office TA" ]
                , Form.form []
                    [ Form.form []
                        [ Form.group []
                            [ InputGroup.config
                                (InputGroup.text [ Input.success, Input.placeholder "username" ])
                                |> InputGroup.predecessors
                                    [ InputGroup.span [] [ text "@" ] ]
                                |> InputGroup.view
                            ]
                        , Form.group []
                            [ InputGroup.config
                                (InputGroup.password [ Input.danger, Input.placeholder "password" ])
                                |> InputGroup.predecessors
                                    [ InputGroup.span [] [ text "*" ] ]
                                |> InputGroup.view
                            , Form.help [] [ text "Minimum 6 characters" ]
                            ]
                        , Button.button [ Button.primary, Button.onClick Submit ] [ text "Sign In" ]
                        ]
                    ]
                ]
            ]
        ]
