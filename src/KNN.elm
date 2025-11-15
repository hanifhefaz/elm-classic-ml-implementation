module KNN exposing
  ( Model
  , fit
  , predict
  , score
  )

{-|
k-Nearest Neighbors (KNN) classifier (pure, lazy learner).
Model stores the training dataset and labels and k value.
-}

import Vector exposing (Vec)
import List exposing (sortBy, take, map)
import Basics exposing (abs)
import Maybe exposing (withDefault)
import Dict exposing (Dict)

type alias Sample =
    { x : Vec
    , y : String
    }


type alias Model =
    { k : Int
    , data : List Sample
    }


-- Fit simply stores training data and parameter k
fit : Int -> List ( Vec, String ) -> Model
fit k rows =
    { k = k
    , data = List.map (\(x, y) -> { x = x, y = y }) rows
    }


-- compute Euclidean distance (consume imported Vector.distance)
distanceWith : (Vec -> Vec -> Float) -> Vec -> Sample -> ( Float, String )
distanceWith distFn x sample =
    ( distFn x sample.x, sample.y )


-- Predict returns (predictedLabel, score)
-- score = proportion of neighbors with that label
predict : (Vec -> Vec -> Float) -> Model -> Vec -> ( String, Float )
predict distFn model x =
    let
        distances =
            List.map (distanceWith distFn x) model.data

        sorted =
            List.sortBy Tuple.first distances

        kNeighbors =
            take model.k sorted

        groups =
            List.foldl
                    (\(_, lbl) acc ->
                        Dict.update lbl (\maybe -> Just (1 + Maybe.withDefault 0 maybe)) acc
                    )
                Dict.empty
                kNeighbors

        best =
            groups
                |> Dict.toList
                |> List.sortBy (\(lbl, count) -> -count)
                |> List.head
                |> Maybe.withDefault ("", 0)

        total =
            List.length kNeighbors |> toFloat

        predScore =
            case best of
                ( _ , c ) -> toFloat c / total
    in
    ( Tuple.first best, predScore )


-- Convenience: score predicted over dataset; expects dataset as list of (x,y)
score : (Vec -> Vec -> Float) -> Model -> List ( Vec, String ) -> Float
score distFn model testset =
    let
        results =
            List.map
                (\(x, yTrue) ->
                    let
                        ( pred, _ ) = predict distFn model x
                    in
                    if pred == yTrue then 1 else 0
                )
                testset
    in
    (List.sum results) / (toFloat (List.length testset))
