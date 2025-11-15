module Regression exposing
  ( LinearModel
  , LogisticModel
  , fitLinearGD
  , predictLinear
  , fitLogisticGD
  , predictLogistic
  )

{-|
Simple linear regression and logistic regression using batch gradient descent.
Features are represented as List Float, bias term handled explicitly.
-}

import List exposing (map, foldl, range)
import Vector exposing (Vec, fromList, toList)


type alias LinearModel =
    { weights : List Float } -- includes bias as last element


type alias LogisticModel =
    { weights : List Float } -- includes bias as last element


-- dot product for lists
dot : List Float -> List Float -> Float
dot a b =
    List.map2 (*) a b |> List.foldl (+) 0.0


-- add bias term
addBias : List Float -> List Float
addBias xs = xs ++ [ 1.0 ]


-- predict linear (y = x.w)
predictLinear : LinearModel -> List Float -> Float
predictLinear model features =
    dot model.weights (addBias features)


-- mean squared error
mse : LinearModel -> List ( List Float, Float ) -> Float
mse model dataset =
    let
        errs =
            List.map (\(x, y) -> let p = predictLinear model x in (p - y) ^ 2) dataset
    in
    (List.sum errs) / toFloat (List.length dataset)


-- Gradient descent for linear regression
fitLinearGD : List ( List Float, Float ) -> { lr : Float, epochs : Int } -> LinearModel
fitLinearGD dataset params =
    let
        nFeatures =
            case dataset of
                (x, _) :: _ -> List.length x
                [] -> 0

        initW = List.repeat (nFeatures + 1) 0.0

        step w =
            let
                updateGrad (x, y) acc =
                    let
                        xBias = addBias x
                        pred = dot w xBias
                        err = pred - y
                        g = List.map (\xi -> 2 * err * xi) xBias
                    in
                    List.map2 (+) acc g

                gradients = List.foldl updateGrad (List.repeat (nFeatures + 1) 0.0) dataset

                n = toFloat (List.length dataset)
                lr = params.lr
                updateW wi gi = wi - lr * (gi / n)
                wPrime = List.map2 updateW w gradients
            in
            wPrime
        finalW =
            List.foldl (\_ acc -> step acc) initW (List.range 1 params.epochs)
    in
    { weights = finalW }


-- Logistic helpers
expApprox : Float -> Float
expApprox x =
    -- simple Taylor series approximation for e^x
    let
        go n (s, tPrev) =
            let
                tNext = tPrev * x / toFloat n
            in
            (s + tNext, tNext)

        (sumApprox, _) = List.foldl go (1.0, 1.0) (List.range 1 15)
    in
    sumApprox


sigmoid : Float -> Float
sigmoid z =
    1.0 / (1.0 + expApprox (-z))


predictLogisticProb : LogisticModel -> List Float -> Float
predictLogisticProb model features =
    sigmoid (dot model.weights (addBias features))


predictLogistic : LogisticModel -> List Float -> ( String, Float )
predictLogistic model features =
    let p = predictLogisticProb model features
    in if p >= 0.5 then ( "1", p ) else ( "0", p )


-- Fit logistic via batch gradient descent (labels as "0" or "1")
fitLogisticGD : List ( List Float, String ) -> { lr : Float, epochs : Int } -> LogisticModel
fitLogisticGD dataset params =
    let
        nFeatures =
            case dataset of
                (x, _) :: _ -> List.length x
                [] -> 0

        initW = List.repeat (nFeatures + 1) 0.0

        step w =
            let
                updateGradLog (x, ystr) acc =
                    let
                        y = if ystr == "1" then 1.0 else 0.0
                        xBias = addBias x
                        pred = sigmoid (dot w xBias)
                        err = pred - y
                        g = List.map (\xi -> err * xi) xBias
                    in
                    List.map2 (+) acc g

                gradients = List.foldl updateGradLog (List.repeat (nFeatures + 1) 0.0) dataset

                n = toFloat (List.length dataset)
                lr = params.lr
                updateW wi gi = wi - lr * (gi / n)
                wPrime = List.map2 updateW w gradients
            in
            wPrime
        finalW =
            List.foldl (\_ acc -> step acc) initW (List.range 1 params.epochs)
    in
    { weights = finalW }
