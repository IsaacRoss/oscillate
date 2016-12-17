port module Main exposing (..)

import Html exposing (Html, div, button, text, section, h1)
import Html.Events exposing (onClick)
import Window
import Task
import Mouse


port audio : Model -> Cmd msg


type alias Model =
    { gainValue : Float
    , frequencyValue : Float
    , windowWidth : Int
    , windowHeight : Int
    }


getInitialWindowSize : Cmd Msg
getInitialWindowSize =
    Task.perform UpdateDimensions Window.size


model : Model
model =
    { gainValue = 0.001
    , frequencyValue = 3000
    , windowWidth = 100
    , windowHeight = 100
    }


type alias WindowDimensions =
    { width : Int, height : Int }


type alias MousePoint =
    ( Int, Int )


type Msg
    = IncrementGain
    | DecrementGain
    | IncrementFrequency
    | DecrementFrequency
    | UpdateDimensions WindowDimensions
    | UpdateMouse MousePoint
    | NoOp


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, getInitialWindowSize )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes UpdateDimensions
        , Mouse.moves (\{ x, y } -> UpdateMouse ( x, y ))
        ]


view : Model -> Html Msg
view model =
    div []
        [ section []
            [ h1 [] [ text "Gain" ]
            , button [ onClick DecrementGain ] [ text "-" ]
            , div [] [ text <| toString <| model.gainValue ]
            , button [ onClick IncrementGain ] [ text "+" ]
            ]
        , section []
            [ h1 [] [ text "Frequency" ]
            , button [ onClick DecrementFrequency ] [ text "-" ]
            , div [] [ text <| toString <| model.frequencyValue ]
            , button [ onClick IncrementFrequency ] [ text "+" ]
            ]
        , section []
            [ h1 [] [ text "Dimensions" ]
            , div [] [ text <| toString <| model.windowHeight ]
            , div [] [ text <| toString <| model.windowWidth ]
            ]
        ]


calculateFromWindow : ( Int, Int ) -> Float -> Float
calculateFromWindow ( a, b ) multiplier =
    (toFloat a) / (toFloat b) * multiplier


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementGain ->
            ( { model | gainValue = model.gainValue + 0.001 }, audio model )

        DecrementGain ->
            ( { model | gainValue = model.gainValue - 0.001 }, audio model )

        IncrementFrequency ->
            ( { model | frequencyValue = model.frequencyValue + 100 }, audio model )

        DecrementFrequency ->
            ( { model | frequencyValue = model.frequencyValue - 100 }, audio model )

        UpdateDimensions { width, height } ->
            ( { model | windowWidth = width, windowHeight = height }, audio model )

        UpdateMouse ( x, y ) ->
            let
                newGain =
                    calculateFromWindow ( x, model.windowWidth ) 0.03

                newFrequency =
                    calculateFromWindow ( y, model.windowHeight ) 6000.0

                newModel =
                    { model | frequencyValue = newFrequency, gainValue = newGain }
            in
                ( newModel, audio newModel )

        NoOp ->
            ( model, Cmd.none )
