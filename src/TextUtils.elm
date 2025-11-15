module TextUtils exposing
  ( tokenize
  , stopWords
  , wordsToHistogram
  , histogramToProbabilities
  )

{-|
Simple text preprocessing utilities:
- tokenize: very lightweight tokenizer
- stopWords: small built-in list (extendable)
- wordsToHistogram: build word->count dict
- histogramToProbabilities: convert counts to probabilities using Laplace smoothing
-}

import String exposing (trim, toLower, split, replace)
import Dict exposing (Dict)
import List exposing (foldl)


-- Basic tokenizer: lowercases, replaces punctuation with spaces, splits by space
tokenize : String -> List String
tokenize s =
    s
        |> toLower
        |> replace "," " "
        |> replace "." " "
        |> replace "?" " "
        |> replace "!" " "
        |> replace ":" " "
        |> replace ";" " "
        |> replace "(" " "
        |> replace ")" " "
        |> replace "\"" " "
        |> replace "'" " "
        |> replace "-" " "
        |> replace "/" " "
        |> trim
        |> split " "
        |> List.filter (\w -> w /= "")


stopWords : List String
stopWords =
    [ "the", "a", "an", "and", "or", "is", "are", "was", "were", "in", "on", "at", "for", "to", "of", "with", "that", "this", "it", "i", "you", "we", "they" ]


wordsToHistogram : List String -> Dict String Int
wordsToHistogram words =
    let
        folder w dict =
            Dict.update w (\maybe -> Just (Maybe.withDefault 0 maybe + 1)) dict
    in
    List.filter (\w -> not (List.member w stopWords)) words
        |> foldl folder Dict.empty


-- Convert histogram to probabilities with Laplace smoothing alpha
histogramToProbabilities : Dict String Int -> Float -> Dict String Float
histogramToProbabilities hist alpha =
    let
        totalCounts =
            hist |> Dict.values |> List.map toFloat |> List.foldl (+) 0.0

        vocabSize =
            hist |> Dict.keys |> List.length |> toFloat

        probFor count =
            (toFloat count + alpha) / (totalCounts + alpha * vocabSize)
    in
    hist |> Dict.map (\_ c -> probFor c)
