module Main exposing (Model, init, main)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- MODEL


type Request
    = Wait
    | Failure String
    | Loading
    | Success String


type alias Model =
    { username : String
    , password : String
    , submited : Bool
    , request : Request
    , alertVisibility : Alert.Visibility
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" False Wait Alert.closed
    , Cmd.none
    )


type Msg
    = Username String
    | Password String
    | Submit
    | GotToken (Result Http.Error String)
    | AlertMsg Alert.Visibility


obtainToken : String -> String -> Cmd Msg
obtainToken username password =
    Http.post
        { url = "http://localhost:3000/api/login"
        , body = Http.jsonBody (toRequestBody username password)
        , expect = Http.expectJson GotToken (D.field "token" D.string)
        }


toRequestBody : String -> String -> E.Value
toRequestBody username password =
    E.object
        [ ( "email", E.string username )
        , ( "password", E.string password )
        ]


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
            , obtainToken model.username model.password
            )

        AlertMsg visibility ->
            ( { model | alertVisibility = visibility }
            , Cmd.none
            )

        GotToken response ->
            case response of
                Ok token ->
                    ( { model
                        | request = Success token
                        , alertVisibility = Alert.closed
                      }
                    , Cmd.none
                    )

                Err error ->
                    case error of
                        Http.BadUrl url ->
                            ( { model
                                | request =
                                    Failure
                                        (String.concat
                                            [ "Bad Url request: "
                                            , url
                                            ]
                                        )
                                , alertVisibility = Alert.shown
                              }
                            , Cmd.none
                            )

                        Http.Timeout ->
                            ( { model
                                | request = Failure "Request timeout"
                                , alertVisibility = Alert.shown
                              }
                            , Cmd.none
                            )

                        Http.NetworkError ->
                            ( { model
                                | request = Failure "Network error"
                                , alertVisibility = Alert.shown
                              }
                            , Cmd.none
                            )

                        Http.BadStatus code ->
                            ( { model
                                | request =
                                    Failure
                                        (String.concat
                                            [ "HTTP Error Code: "
                                            , String.fromInt code
                                            ]
                                        )
                                , alertVisibility = Alert.shown
                              }
                            , Cmd.none
                            )

                        Http.BadBody _ ->
                            ( { model
                                | request = Failure "Bad response body"
                                , alertVisibility = Alert.shown
                              }
                            , Cmd.none
                            )



-- VIEW


getErrorMessage : Request -> String
getErrorMessage request =
    case request of
        Failure message ->
            message

        _ ->
            ""


view : Model -> Html Msg
view model =
    div []
        [ Alert.config
            |> Alert.warning
            |> Alert.dismissable AlertMsg
            |> Alert.children
                [ Alert.h6 [] [ text (getErrorMessage model.request) ] ]
            |> Alert.view model.alertVisibility
        , Grid.container []
            [ Grid.row [ Row.centerMd, Row.middleXs ]
                [ Grid.col
                    [ Col.sm4 ]
                    [ h3
                        []
                        [ text "Back Office TA" ]
                    , Form.form []
                        [ Form.group []
                            [ InputGroup.config
                                (InputGroup.text
                                    [ Input.success
                                    , Input.placeholder "username"
                                    , Input.value model.username
                                    , Input.onInput Username
                                    ]
                                )
                                |> InputGroup.predecessors
                                    [ InputGroup.span [] [ text "@" ] ]
                                |> InputGroup.view
                            ]
                        , Form.group []
                            [ InputGroup.config
                                (InputGroup.password
                                    [ Input.danger
                                    , Input.placeholder "password"
                                    , Input.value model.password
                                    , Input.onInput Password
                                    ]
                                )
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
