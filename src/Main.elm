module Main exposing (main)

import Browser
import Html exposing (Html, div, text, button, h2, p, ul, li)
import Html.Events exposing (onClick)
import Vector exposing (fromList, toList)
import String exposing (fromFloat, join)
import KMeans
import NaiveBayes
import TextUtils exposing (tokenize)
import Dataset
import KNN
import Regression
import Metrics exposing (accuracy, classificationReport)
import List exposing (map)


type alias Model =
    { kmeansResult : Maybe KMeans.ClusterResult
    , nbResult : Maybe ( String, Float )
    , knnResult : Maybe ( String, Float )
    , linearResult : Maybe Float
    , logisticResult : Maybe ( String, Float )
    , metricsText : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { kmeansResult = Nothing, nbResult = Nothing, knnResult = Nothing, linearResult = Nothing, logisticResult = Nothing, metricsText = "" }
    , Cmd.none
    )


type Msg
    = RunKMeans
    | RunNB
    | RunKNN
    | RunLinear
    | RunLogistic
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RunKMeans ->
            let
                dataset = List.head [ Dataset.toNumericIris Dataset.irisSmall |> List.map Tuple.first ] |> Maybe.withDefault []
                numeric = Dataset.toNumericIris Dataset.irisSmall |> List.map Tuple.first
                vecs = numeric
                res = KMeans.kMeans 2 20 vecs
            in
            ( { model | kmeansResult = Just res }, Cmd.none )

        RunNB ->
            let
                dataset = Dataset.spamSmall
                modelNb = NaiveBayes.fit 1.0 dataset
                pred = NaiveBayes.predict modelNb "you won a free prize"
            in
            ( { model | nbResult = Just pred }, Cmd.none )

        RunKNN ->
            let
                training = Dataset.toNumericIris Dataset.irisSmall
                modelK = KNN.fit 3 training
                sample = fromList [6.0, 3.0]
                (lbl, sc) = KNN.predict Vector.distance modelK sample
            in
            ( { model | knnResult = Just (lbl, sc) }, Cmd.none )

        RunLinear ->
            let
                -- tiny regression dataset: y ~ x0 + 2*x1
                ds =
                    [ ( [1.0, 2.0], 5.0 )
                    , ( [2.0, 1.0], 4.0 )
                    , ( [3.0, 4.0], 11.0 )
                    ]

                modelLin = Regression.fitLinearGD ds { lr = 0.01, epochs = 2000 }
                pred = Regression.predictLinear modelLin [2.0, 2.0]
            in
            ( { model | linearResult = Just pred }, Cmd.none )

        RunLogistic ->
            let
                ds =
                    [ ( [0.0, 0.0], "0" )
                    , ( [0.0, 1.0], "0" )
                    , ( [1.0, 0.0], "1" )
                    , ( [1.0, 1.0], "1" )
                    ]
                modelLog = Regression.fitLogisticGD ds { lr = 0.5, epochs = 500 }
                (lbl, p) = Regression.predictLogistic modelLog [1.0, 0.0]
            in
            ( { model | logisticResult = Just (lbl, p) }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "ELM ML DEMO" ]
        , div [] [ button [ onClick RunKMeans ] [ text "Run k-means demo" ] ]
        , div [] [ button [ onClick RunNB ] [ text "Run Naive Bayes demo" ] ]
        , div [] [ button [ onClick RunKNN ] [ text "Run KNN demo" ] ]
        , div [] [ button [ onClick RunLinear ] [ text "Run Linear Regression demo" ] ]
        , div [] [ button [ onClick RunLogistic ] [ text "Run Logistic Regression demo" ] ]
        , div [] (
            case model.kmeansResult of
                Nothing -> [ p [] [ text "k-means: not run yet." ] ]
                Just (KMeans.ClusterResult r) ->
                    [ p [] [ text ("k-means iterations: " ++ String.fromInt r.iterations) ]
                    , p [] [ text ("centroids count: " ++ String.fromInt (List.length r.centroids)) ]
                    , ul [] (List.map (\c -> li [] [ text (join ", " (List.map fromFloat (toList c))) ]) r.centroids)
                    ]
          )
        , div [] (
            case model.nbResult of
                Nothing -> [ p [] [ text "Naive Bayes: not run yet." ] ]
                Just (label, score) -> [ p [] [ text ("NB Predicted: " ++ label ++ " (score: " ++ String.fromFloat score ++ ")") ] ]
          )
        , div [] (
            case model.knnResult of
                Nothing -> [ p [] [ text "KNN: not run yet." ] ]
                Just (label, score) -> [ p [] [ text ("KNN Predicted: " ++ label ++ " (score: " ++ String.fromFloat score ++ ")") ] ]
          )
        , div [] (
            case model.linearResult of
                Nothing -> [ p [] [ text "Linear: not run yet." ] ]
                Just v -> [ p [] [ text ("Linear predicted (example): " ++ String.fromFloat v) ] ]
          )
        , div [] (
            case model.logisticResult of
                Nothing -> [ p [] [ text "Logistic: not run yet." ] ]
                Just (l, prob) -> [ p [] [ text ("Logistic predicted: " ++ l ++ " (prob: " ++ String.fromFloat prob ++ ")") ] ]
          )
        ]
        

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


main =
        Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }
