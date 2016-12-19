port module Main exposing (..)

import Html exposing (Html, div, button, text, section, h1)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, type_)
import Element exposing (Element)
import Collage exposing (collage, path, traced, solid, move)
import Color exposing (..)
import Window
import Task
import Mouse


port audio : Model -> Cmd msg


port visualization : (List Int -> msg) -> Sub msg


type alias Model =
    { gainValue : Float
    , frequencyValue : Float
    , windowWidth : Int
    , windowHeight : Int
    , visualizationData : List Int
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
    , visualizationData = []
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
    | Visualization (List Int)
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
        , visualization Visualization
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
        , div []
            [ div [ class "visualization" ]
                [ (visualizationGraph model) |> Element.toHtml ]
            ]
        ]


visualizationGraph : Model -> Element
visualizationGraph model =
    let
        points =
            (toPoints model)
    in
        collage model.windowWidth
            model.windowHeight
            [ path points
                |> traced (solid red)
                |> move
                    ( (toFloat model.windowWidth) / -2
                    , (toFloat model.windowHeight) / -2
                    )
            ]


toPoints : Model -> List ( Float, Float )
toPoints model =
    let
        sliceWidth =
            (toFloat model.windowWidth) / (toFloat (List.length model.visualizationData))

        indexedDatumToPoint n datum =
            let
                v =
                    (toFloat datum) / 128

                y =
                    (v * (toFloat model.windowHeight)) / 2

                x =
                    sliceWidth * (toFloat n)
            in
                ( x, y )
    in
        model.visualizationData
            |> List.indexedMap indexedDatumToPoint


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

        Visualization data ->
            ( { model | visualizationData = data }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )
