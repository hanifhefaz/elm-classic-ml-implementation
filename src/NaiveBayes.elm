module NaiveBayes exposing
    ( Model
    , fit
    , predict
    )

{-|
A small Multinomial Naive Bayes implementation for text classification.
It uses TextUtils.tokenize and wordsToHistogram internally.
-}

import Dict exposing (Dict)
import List exposing (foldl)
import Maybe exposing (withDefault)
import TextUtils exposing (tokenize, wordsToHistogram, histogramToProbabilities)
import Basics exposing (logBase, toFloat)


type alias Model =
    { priors : Dict String Float
    , condProb : Dict String (Dict String Float) -- class -> (word -> prob)
    , vocab : Dict String Bool
    , alpha : Float
    }


{-|
Fit model.
dataset : List ( text, classLabel )
alpha : Laplace smoothing constant

alternative fn under process...
-}
fit : Float -> List ( String, String ) -> Model
fit alpha dataset =
    let
        -- returns (classCounts, wordCountsDict, vocab)
        folder ( text, cls ) ( classCounts, wordCounts, vocab ) =
            let
                toks = tokenize text
                hist = wordsToHistogram toks

                classCountsPrime =
                    Dict.update cls (\maybe -> Just (withDefault 0 maybe + 1)) classCounts

                wordCountsPrime =
                    Dict.update cls
                        (\maybe ->
                            let
                                prev = withDefault Dict.empty maybe
                                merged =
                                    -- merge two Dict String Int by summing values
                                    Dict.foldl
                                        (\k v acc -> Dict.update k (\m -> Just (v + withDefault 0 m)) acc)
                                        hist
                                        prev
                            in
                            Just merged
                        )
                        wordCounts

                vocabPrime =
                    hist
                        |> Dict.keys
                        |> List.foldl (\w v -> Dict.insert w True v) vocab
            in
            ( classCountsPrime, wordCountsPrime, vocabPrime )

        ( accClassCounts, accWordCounts, accVocab ) =
            List.foldl folder ( Dict.empty, Dict.empty, Dict.empty ) dataset

        totalDocs =
            accClassCounts |> Dict.values |> List.map toFloat |> List.foldl (+) 0.0

        priors =
            Dict.map (\_ c -> (toFloat c) / totalDocs) accClassCounts

        condProb =
            Dict.map (\_ hist -> histogramToProbabilities hist alpha) accWordCounts
    in
    { priors = priors
    , condProb = condProb
    , vocab = accVocab
    , alpha = alpha
    }


{-|
Predict returns (bestClass, score) where score is a log-probability approximation.
-}
predict : Model -> String -> ( String, Float )
predict model text =
    let
        tokens =
            tokenize text |> List.filter (\w -> Dict.member w model.vocab)

        classes = Dict.keys model.priors

        maxBy proj list =
            case list of
                [] ->
                    Nothing
                x :: xs ->
                    Just <|
                        List.foldl
                            (\y best -> if proj y > proj best then y else best)
                            x
                            xs

        scoreFor cls =
            let
                prior = Dict.get cls model.priors |> withDefault 1e-12
                logPrior = logBase 10 prior
                cond = Dict.get cls model.condProb |> withDefault Dict.empty

                tokenLogSum =
                    List.foldl
                        (\tok acc ->
                            let p = Dict.get tok cond |> withDefault 1e-12
                            in
                            acc + logBase 10 p
                        )
                        0.0
                        tokens
            in
            ( cls, logPrior + tokenLogSum )
    in
    classes
        |> List.map scoreFor
        |> maxBy (\(_, s) -> s)
        |> Maybe.withDefault ("", -1e12)
