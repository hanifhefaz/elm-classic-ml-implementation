module Metrics exposing
  ( accuracy
  , confusionMatrix
  , precision
  , recall
  , f1
  , classificationReport
  )

{-|
Basic classification metrics for binary or multi-class classification.
Confusion matrix is returned as Dict (trueLabel, predictedLabel) -> count
-}

import Dict exposing (Dict)
import List exposing (foldl)
import Maybe exposing (withDefault)


type alias Example =
    { yTrue : String
    , yPred : String
    }


accuracy : List Example -> Float
accuracy examples =
    let
        correct =
            List.foldl (\e acc -> if e.yTrue == e.yPred then acc + 1 else acc) 0 examples
    in
    toFloat correct / toFloat (List.length examples)


confusionMatrix : List Example -> Dict ( String, String ) Int
confusionMatrix examples =
    List.foldl
        (\e acc ->
            let key = ( e.yTrue, e.yPred )
            in Dict.update key (\m -> Just (1 + withDefault 0 m)) acc
        )
        Dict.empty
        examples


-- Precision for a single class = TP / (TP + FP)
precision : String -> List Example -> Float
precision cls examples =
    let
        tp =
            List.foldl (\e acc -> if e.yTrue == cls && e.yPred == cls then acc + 1 else acc) 0 examples

        fp =
            List.foldl (\e acc -> if e.yTrue /= cls && e.yPred == cls then acc + 1 else acc) 0 examples
    in
    if tp + fp == 0 then 0 else toFloat tp / toFloat (tp + fp)


-- Recall for a single class = TP / (TP + FN)
recall : String -> List Example -> Float
recall cls examples =
    let
        tp =
            List.foldl (\e acc -> if e.yTrue == cls && e.yPred == cls then acc + 1 else acc) 0 examples

        fn =
            List.foldl (\e acc -> if e.yTrue == cls && e.yPred /= cls then acc + 1 else acc) 0 examples
    in
    if tp + fn == 0 then 0 else toFloat tp / toFloat (tp + fn)


-- F1 for a class
f1 : String -> List Example -> Float
f1 cls examples =
    let
        p = precision cls examples
        r = recall cls examples
    in
    if p + r == 0 then 0 else 2.0 * (p * r) / (p + r)


-- Classification report: for each class, compute precision, recall, f1, support
type alias ClassReport =
    { className : String
    , precision : Float
    , recall : Float
    , f1 : Float
    , support : Int
    }

classificationReport : List String -> List Example -> List ClassReport
classificationReport classes examples =
    List.map
        (\cls ->
            let
                p = precision cls examples
                r = recall cls examples
                f = f1 cls examples
                support = List.foldl (\e acc -> if e.yTrue == cls then acc + 1 else acc) 0 examples
            in
            { className = cls, precision = p, recall = r, f1 = f, support = support }
        )
        classes
