module DemoKNN exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, button, text, input)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import SvgHelpers
import Vector exposing (Vec, toList, fromList)
import Dataset exposing (toNumericIris, irisSmall)
import KNN
import List exposing (map)


type alias Model =
    { k : Int
    , dataset : List ( Vec, String )
    , query : ( Float, Float )
    , pred : Maybe ( String, Float )
    , domain : { xMin : Float, xMax : Float, yMin : Float, yMax : Float }
    }


type Msg
    = SetK String
    | SetQueryX String
    | SetQueryY String
    | Run
    | Clear



init : Model
init =
    let
        ds = Dataset.toNumericIris Dataset.irisSmall
    in
    { k = 3
    , dataset = ds
    , query = ( 4, 3 )
    , pred = Nothing
    , domain = { xMin = 0, xMax = 8, yMin = 0, yMax = 6 }
    }



update : Msg -> Model -> Model
update msg model =
    case msg of
        SetK s ->
            case String.toInt s of
                Just n ->
                    { model | k = max 1 n }

                Nothing ->
                    model

        SetQueryX s ->
            case String.toFloat s of
                Just v ->
                    { model | query = ( v, Tuple.second model.query ) }

                Nothing ->
                    model

        SetQueryY s ->
            case String.toFloat s of
                Just v ->
                    { model | query = ( Tuple.first model.query, v ) }

                Nothing ->
                    model

        Run ->
            let
                knn =
                    KNN.fit model.k model.dataset

                qvec =
                    fromList [ Tuple.first model.query, Tuple.second model.query ]

                ( lbl, score ) =
                    KNN.predict Vector.distance knn qvec
            in
            { model | pred = Just ( lbl, score ) }

        Clear ->
            { model | pred = Nothing }



colorOf : String -> String
colorOf lbl =
    case lbl of
        "setosa" ->
            "green"

        "versicolor" ->
            "blue"

        "virginica" ->
            "purple"

        _ ->
            "gray"



view : Model -> Html Msg
view model =
    let
        w = 600
        h = 400

        pts =
            List.map
                (\( v, lbl ) ->
                    case toList v of
                        x :: y :: _ ->
                            SvgHelpers.viewPoint model.domain w h ( x, y ) (colorOf lbl)

                        _ ->
                            SvgHelpers.viewPoint model.domain w h ( 0, 0 ) "gray"
                )
                model.dataset

        queryPt =
            SvgHelpers.viewPoint model.domain w h model.query "red"

        predText =
            case model.pred of
                Nothing ->
                    text "Prediction: -"

                Just ( lbl, score ) ->
                    text ("Prediction: " ++ lbl ++ " (score " ++ String.fromFloat score ++ ")")
    in
    div []
        [ div []
            [ text "k: "
            , input [ type_ "number", value (String.fromInt model.k), onInput SetK ] []
            , text "   Query X: "
            , input [ type_ "number", value (String.fromFloat (Tuple.first model.query)), onInput SetQueryX ] []
            , text "   Query Y: "
            , input [ type_ "number", value (String.fromFloat (Tuple.second model.query)), onInput SetQueryY ] []
            , button [ onClick Run ] [ text "Predict" ]
            , button [ onClick Clear ] [ text "Clear" ]
            ]
        , div []
            [ SvgHelpers.viewCanvas w h (pts ++ [ queryPt ]) ]
        , div []
            [ predText ]
        ]
