module DemoRegression exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import SvgHelpers
import Regression
import Vector exposing (toList)
import Dataset exposing (irisSmall, toNumericIris)


type alias Model =
    { linearPred : Maybe Float
    , logisticPred : Maybe ( String, Float )
    , domain : { xMin : Float, xMax : Float, yMin : Float, yMax : Float }
    }


type Msg
    = RunLinear
    | RunLogistic



init : Model
init =
    { linearPred = Nothing
    , logisticPred = Nothing
    , domain = { xMin = 0, xMax = 8, yMin = 0, yMax = 6 }
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        RunLinear ->
            let
                ds =
                    [ ( [ 1, 2 ], 5 )
                    , ( [ 2, 1 ], 4 )
                    , ( [ 3, 4 ], 11 )
                    ]

                m =
                    Regression.fitLinearGD ds { lr = 0.01, epochs = 2000 }

                p =
                    Regression.predictLinear m [ 2, 2 ]
            in
            { model | linearPred = Just p }

        RunLogistic ->
            let
                ds =
                    [ ( [ 0, 0 ], "0" )
                    , ( [ 0, 1 ], "0" )
                    , ( [ 1, 0 ], "1" )
                    , ( [ 1, 1 ], "1" )
                    ]

                m =
                    Regression.fitLogisticGD ds { lr = 0.5, epochs = 500 }

                ( lbl, prob ) =
                    Regression.predictLogistic m [ 1, 0 ]
            in
            { model | logisticPred = Just ( lbl, prob ) }


view : Model -> Html Msg
view model =
    let
        w = 600
        h = 400

        pts =
            Dataset.toNumericIris Dataset.irisSmall
                |> List.map
                    (\( v, lbl ) ->
                        case toList v of
                            x :: y :: _ ->
                                SvgHelpers.viewPoint model.domain w h ( x, y ) "steelblue"

                            _ ->
                                SvgHelpers.viewPoint model.domain w h ( 0, 0 ) "gray"
                    )

        linearText =
            case model.linearPred of
                Nothing ->
                    text "Linear: (not run)"

                Just p ->
                    text ("Linear prediction example: " ++ String.fromFloat p)

        logisticText =
            case model.logisticPred of
                Nothing ->
                    text "   Logistic: (not run)"

                Just ( lbl, prob ) ->
                    text ("   Logistic: " ++ lbl ++ " (p=" ++ String.fromFloat prob ++ ")")
    in
    div []
        [ div []
            [ button [ onClick RunLinear ] [ text "Run Linear Regression" ]
            , button [ onClick RunLogistic ] [ text "Run Logistic Regression" ]
            ]
        , div [] [ SvgHelpers.viewCanvas w h pts ]
        , div [] [ linearText, text "   ", logisticText ]
        ]
