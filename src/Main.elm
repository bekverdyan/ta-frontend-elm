port module Main exposing (Model, init, main)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Utilities.Spacing as Spacing
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



-- PORT


port auth : E.Value -> Cmd msg


port navigateTo : E.Value -> Cmd msg



-- MODEL


type alias AuthResult =
    { name : String
    , role : String
    , roles : List Role
    , accountExpired : Bool
    }


type alias Role =
    ( String, Bool )


type alias Model =
    { username : String
    , password : String
    , request : Request
    , alertVisibility : Alert.Visibility
    }


type Request
    = NotSentYet
    | Failure Reason
    | Loading
    | Success


type Reason
    = Unauthorized
    | Other String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" NotSentYet Alert.closed
    , Cmd.none
    )


obtainToken : String -> String -> Cmd Msg
obtainToken username password =
    Http.post
        { url = "http://localhost:8080/OOB-PRODUCT-BO-TA/rest/login/authenticate"
        , body = Http.jsonBody (encodeRequestBody username password)
        , expect = Http.expectJson GotToken decodeAuthResult
        }



-- TODO IMPLEMENT ME


decodeAuthResult : D.Decoder AuthResult
decodeAuthResult =
    D.map4 AuthResult
        (D.field "name" D.string)
        (D.field "role" D.string)
        (D.field "roles" decodeRole)
        (D.field "accountExpired" D.bool)


decodeRole : D.Decoder (List ( String, Bool ))
decodeRole =
    D.keyValuePairs D.bool


encodeAuthResult : AuthResult -> E.Value
encodeAuthResult authResult =
    E.object
        [ ( "name", E.string authResult.name )
        , ( "role", E.string authResult.role )
        , ( "roles", E.list encodeRole authResult.roles )
        , ( "accountExpired", E.bool authResult.accountExpired )
        ]


encodeRole : Role -> E.Value
encodeRole ( name, indicator ) =
    E.object
        [ ( name, E.bool indicator ) ]


encodeRequestBody : String -> String -> E.Value
encodeRequestBody username password =
    E.object
        [ ( "username", E.string username )
        , ( "password", E.string password )
        ]



-- UPDATE


type Msg
    = Username String
    | Password String
    | Submit
    | GotToken (Result Http.Error AuthResult)
    | AlertMsg Alert.Visibility
    | ForgotPassword


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Username username ->
            ( { model | username = username }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        Submit ->
            ( { model | alertVisibility = Alert.closed }
            , obtainToken model.username model.password
            )

        AlertMsg visibility ->
            ( { model | alertVisibility = visibility }, Cmd.none )

        GotToken response ->
            handleResponse response model

        ForgotPassword ->
            ( model, navigateTo (E.object [ ( "url", E.string "forgotPassword" ) ]) )


handleResponse : Result Http.Error AuthResult -> Model -> ( Model, Cmd Msg )
handleResponse response model =
    case response of
        Ok authResult ->
            ( { model
                | request = Success
                , alertVisibility = Alert.closed
              }
            , auth <| encodeAuthResult authResult
            )

        Err error ->
            case error of
                Http.BadUrl url ->
                    ( { model
                        | request = Failure <| Other <| "Bad url: " ++ url
                        , alertVisibility = Alert.shown
                      }
                    , Cmd.none
                    )

                Http.Timeout ->
                    ( { model
                        | request = Failure <| Other "Request timeout"
                        , alertVisibility = Alert.shown
                      }
                    , Cmd.none
                    )

                Http.NetworkError ->
                    ( { model
                        | request = Failure <| Other "Network error"
                        , alertVisibility = Alert.shown
                      }
                    , Cmd.none
                    )

                Http.BadStatus code ->
                    handleStatusCode code model

                Http.BadBody _ ->
                    ( { model
                        | request = Failure <| Other "Unexpected content received"
                        , alertVisibility = Alert.shown
                      }
                    , Cmd.none
                    )


handleStatusCode : Int -> Model -> ( Model, Cmd Msg )
handleStatusCode code model =
    case code of
        401 ->
            ( { model
                | request = Failure Unauthorized
                , alertVisibility = Alert.shown
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


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
                                (InputGroup.text <|
                                    viewInput model.request "username" model.username Username
                                )
                                |> InputGroup.predecessors
                                    [ InputGroup.span [] [ text "@" ] ]
                                |> InputGroup.view
                            ]
                        , Form.group []
                            [ InputGroup.config
                                (InputGroup.password <|
                                    viewInput model.request "password" model.password Password
                                )
                                |> InputGroup.predecessors
                                    [ InputGroup.span [] [ text "*" ] ]
                                |> InputGroup.view
                            , Form.help [] [ text "Minimum 6 characters" ]
                            ]
                        , Grid.row
                            [ Row.betweenXs ]
                            [ Grid.col []
                                [ Button.button
                                    [ Button.primary
                                    , Button.onClick Submit
                                    ]
                                    [ text "Sign In" ]
                                ]
                            , Grid.col []
                                [ Button.button
                                    [ Button.roleLink
                                    , Button.onClick ForgotPassword
                                    ]
                                    [ text "Forgot password" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


getErrorMessage : Request -> String
getErrorMessage request =
    case request of
        Failure type_ ->
            case type_ of
                Other message ->
                    message

                Unauthorized ->
                    "Invalid USERNAME or PASSWORD"

        _ ->
            ""


viewInput : Request -> String -> String -> (String -> Msg) -> List (Input.Option Msg)
viewInput request placeholder value command =
    let
        regularInput =
            [ Input.placeholder placeholder
            , Input.value value
            , Input.onInput command
            ]
    in
    case request of
        Failure type_ ->
            case type_ of
                Unauthorized ->
                    Input.danger :: regularInput

                _ ->
                    regularInput

        _ ->
            regularInput
