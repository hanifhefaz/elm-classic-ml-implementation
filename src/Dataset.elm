module Dataset exposing
    ( irisSmall
    , spamSmall
    , toNumericIris
    , toNumericIrisNormalized
    )

{-|
Small built-in datasets for demos:
- irisSmall : tiny subset of iris as (features, label)
- spamSmall : tiny SMS spam-like dataset for Naive Bayes (text,label)
- toNumericIris : helper to convert iris list of lists to Vector arrays
-}

import Vector exposing (Vec, fromList)
import List exposing (map, foldl, map2)
import Basics exposing (min, max)


-- Very small Iris-like sample (features: sepal length, sepal width)
irisSmall : List ( List Float, String )
irisSmall =
    [ ( [5.1, 3.5], "setosa" )
    , ( [4.9, 3.0], "setosa" )
    , ( [5.0, 3.6], "setosa" )
    , ( [5.4, 3.9], "setosa" )
    , ( [6.2, 2.9], "versicolor" )
    , ( [5.9, 3.0], "versicolor" )
    , ( [6.0, 2.2], "versicolor" )
    , ( [6.9, 3.1], "virginica" )
    , ( [7.1, 3.0], "virginica" )
    , ( [7.6, 3.0], "virginica" )
    ]


toNumericIris : List ( List Float, String ) -> List ( Vec, String )
toNumericIris xs =
    List.map (\(f, lbl) -> ( fromList f, lbl )) xs


{-|
toNumericIrisNormalized: convert a list of feature lists to numeric Vectors and
normalize each feature to [0,1] across the dataset.
-}
toNumericIrisNormalized : List ( List Float, String ) -> List ( Vec, String )
toNumericIrisNormalized xs =
    let
        features = List.map Tuple.first xs

        featureStats rows =
            case rows of
                [] ->
                    []

                firstRow :: restRows ->
                    let
                        featureCount = List.length firstRow

                        getValAt i row =
                            case List.drop i row |> List.head of
                                Just v -> v
                                Nothing -> 0.0

                        minAt i =
                            let
                                firstVal = getValAt i firstRow
                            in
                            List.foldl
                                (\r acc -> let v = getValAt i r in if v < acc then v else acc)
                                firstVal
                                restRows

                        maxAt i =
                            let
                                firstVal = getValAt i firstRow
                            in
                            List.foldl
                                (\r acc -> let v = getValAt i r in if v > acc then v else acc)
                                firstVal
                                restRows

                    in
                    List.map (\i -> ( minAt i, maxAt i )) (List.range 0 (featureCount - 1))

        stats = featureStats features

        normalizeRow row =
            List.map2
                (\v (mn, mx) ->
                    if mx == mn then
                        0.5
                    else
                        (v - mn) / (mx - mn)
                )
                row
                stats

    in
    List.map (\(f, lbl) -> ( fromList (normalizeRow f), lbl )) xs


spamSmall : List ( String, String )
spamSmall =
    [ ( "Free entry win money", "spam" )
    , ( "Congratulations you have won a prize", "spam" )
    , ( "Call me when you can", "ham" )
    , ( "Are we meeting today", "ham" )
    ]
