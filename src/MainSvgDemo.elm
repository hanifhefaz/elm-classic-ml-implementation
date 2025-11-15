module MainSvgDemo exposing (main)


import Browser
import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import DemoKMeans
import DemoKNN
import DemoNaiveBayes
import DemoRegression
import Metrics exposing (accuracy, classificationReport)
import String exposing (fromFloat)
import KNN
import NaiveBayes
import Regression
import Dataset
import Vector exposing (toList)
import List exposing (foldl, indexedMap, map, any, filter, reverse)
import Svg exposing (Svg)


type Demo
    = Home
    | KMeansDemo DemoKMeans.Model
    | KNNDemo DemoKNN.Model
    | NBDemo DemoNaiveBayes.Model
    | RegDemo DemoRegression.Model


type alias Model =
    { page : Demo
    }


type Msg
    = GoHome
    | StartKMeans
    | StartKNN
    | StartNB
    | StartReg
    | KMeansMsg DemoKMeans.Msg
    | KNNMsg DemoKNN.Msg
    | NBMsg DemoNaiveBayes.Msg
    | RegMsg DemoRegression.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Home }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoHome ->
            ( { model | page = Home }, Cmd.none )

        StartKMeans ->
            ( { model | page = KMeansDemo DemoKMeans.init }, Cmd.none )

        StartKNN ->
            ( { model | page = KNNDemo DemoKNN.init }, Cmd.none )

        StartNB ->
            ( { model | page = NBDemo DemoNaiveBayes.init }, Cmd.none )

        StartReg ->
            ( { model | page = RegDemo DemoRegression.init }, Cmd.none )

        KMeansMsg m ->
            case model.page of
                KMeansDemo s ->
                    let ns = DemoKMeans.update m s in
                    ( { model | page = KMeansDemo ns }, Cmd.none )

                _ -> ( model, Cmd.none )

        KNNMsg m ->
            case model.page of
                KNNDemo s ->
                    let ns = DemoKNN.update m s in
                    ( { model | page = KNNDemo ns }, Cmd.none )

                _ -> ( model, Cmd.none )

        NBMsg m ->
            case model.page of
                NBDemo s ->
                    let ns = DemoNaiveBayes.update m s in
                    ( { model | page = NBDemo ns }, Cmd.none )

                _ -> ( model, Cmd.none )

        RegMsg m ->
            case model.page of
                RegDemo s ->
                    let ns = DemoRegression.update m s in
                    ( { model | page = RegDemo ns }, Cmd.none )

                _ -> ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        nav =
            div []
                [ button [ onClick StartKMeans ] [ text "k-Means" ]
                , button [ onClick StartKNN ] [ text "KNN" ]
                , button [ onClick StartNB ] [ text "Naïve Bayes" ]
                , button [ onClick StartReg ] [ text "Regression" ]
                ]

        instructions demo =
            case demo of
                Home ->
                    div [] [ text "Pick a demo from the navigation. Each demo provides interactive controls and a visualization." ]

                KMeansDemo _ ->
                    div []
                        [ text "k-Means: cluster unlabeled 2D points. Adjust k (number of clusters) and max iterations. Points are colored by cluster; centroids are shown as highlighted points." ]

                KNNDemo _ ->
                    div []
                        [ text "KNN (k-nearest neighbors): given a query point, the demo finds the k nearest labeled points and predicts a label by majority vote. Adjust k or the query point to see how predictions change." ]

                NBDemo _ ->
                    div []
                        [ text "Naive Bayes (text): small demo that trains a multinomial Naive Bayes model on a tiny spam/ham dataset. It classifies short messages and shows a score (log-probability)." ]

                RegDemo _ ->
                    div []
                        [ text "Regression: demonstrates linear regression (continuous target) and logistic regression (binary classification) using simple datasets and gradient descent." ]

        distinctLabels ds =
            ds
                |> List.foldl (\(_, lbl) acc -> if List.any ((==) lbl) acc then acc else lbl :: acc) []
                |> List.reverse

        knnMetrics s =
            let
                indexed = List.indexedMap (\i v -> ( i, v )) s.dataset
                examples =
                    indexed
                        |> List.map
                            (\(i, (v, lbl)) ->
                                let
                                    training = indexed |> List.filter (\(j, _) -> j /= i) |> List.map Tuple.second
                                    knnModel = KNN.fit s.k training
                                    (pred, _) = KNN.predict Vector.distance knnModel v
                                in
                                { yTrue = lbl, yPred = pred }
                            )
                acc = accuracy examples
                classes = distinctLabels s.dataset
                report = classificationReport classes examples
            in
            div []
                [ text ("KNN leave-one-out accuracy: " ++ String.fromFloat acc)
                , div [] (List.map (\r -> div [] [ text (r.className ++ ": p=" ++ String.fromFloat r.precision ++ ", r=" ++ String.fromFloat r.recall ++ ", f1=" ++ String.fromFloat r.f1) ]) report)
                ]

        nbMetrics s =
            let
                ds = Dataset.spamSmall
                nbModel = NaiveBayes.fit 1.0 ds
                examples = List.map (\(txt, lbl) -> let (pred, _) = NaiveBayes.predict nbModel txt in { yTrue = lbl, yPred = pred }) ds
                acc = accuracy examples
                classes = distinctLabels (List.map (\(t, l) -> (t, l)) ds)
                report = classificationReport classes examples
            in
            div []
                [ text ("Naive Bayes accuracy (train/test same small set): " ++ String.fromFloat acc)
                , div [] (List.map (\r -> div [] [ text (r.className ++ ": p=" ++ String.fromFloat r.precision ++ ", r=" ++ String.fromFloat r.recall ++ ", f1=" ++ String.fromFloat r.f1) ]) report)
                ]

        logisticMetrics s =
            let
                ds = [ ( [ 0, 0 ], "0" ), ( [ 0, 1 ], "0" ), ( [ 1, 0 ], "1" ), ( [ 1, 1 ], "1" ) ]
                logModel = Regression.fitLogisticGD ds { lr = 0.5, epochs = 500 }
                examples = List.map (\(x, lbl) -> let (pred, _) = Regression.predictLogistic logModel x in { yTrue = lbl, yPred = pred }) ds
                acc = accuracy examples
                classes = distinctLabels (List.map (\(x, l) -> (x, l)) ds)
                report = classificationReport classes examples
            in
            div []
                [ text ("Logistic regression accuracy (toy dataset): " ++ String.fromFloat acc)
                , div [] (List.map (\r -> div [] [ text (r.className ++ ": p=" ++ String.fromFloat r.precision ++ ", r=" ++ String.fromFloat r.recall ++ ", f1=" ++ String.fromFloat r.f1) ]) report)
                ]
    in
    case model.page of
        Home ->
            div []
                [ nav
                , div [] [ text "Please select an algorithm first!" ]
                ]

        KMeansDemo s ->
            div [] [ nav, instructions (KMeansDemo s), Html.map KMeansMsg (DemoKMeans.view s), div [] [ text "No supervised metrics for k-means (unsupervised). Use visualization." ] ]

        KNNDemo s ->
            div [] [ nav, instructions (KNNDemo s), Html.map KNNMsg (DemoKNN.view s), knnMetrics s ]

        NBDemo s ->
            div [] [ nav, instructions (NBDemo s), Html.map NBMsg (DemoNaiveBayes.view s), nbMetrics s ]

        RegDemo s ->
            div [] [ nav, instructions (RegDemo s), Html.map RegMsg (DemoRegression.view s), logisticMetrics s ]


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }
