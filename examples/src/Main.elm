module Main exposing (main)

import Browser
import GoalGenerated
import GoalPorts
import Html exposing (..)
import Html.Events
import Json.Decode
import Ports


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { draft : String
    , messages : List String
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    ( { draft = "", messages = [] }
    , Cmd.none
    )


type Msg
    = SendAlert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendAlert ->
            ( model
              --, GeneratedPort.sendPort (Ports.Alert "Hello!")
            , GoalGenerated.alert "Hi!!!"
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Echo Chat" ]
        , button [ Html.Events.onClick SendAlert ] [ text "Alert" ]
        ]
