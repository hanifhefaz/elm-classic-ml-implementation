module KMeans exposing
  ( DataSet
  , kMeans
  , ClusterResult(..)
  )


import Vector exposing (Vec, toList, distance, mean, zeros)
import List exposing (range, foldl, map, indexedMap)
import Dict exposing (Dict)
import Maybe exposing (withDefault)


type alias DataSet =
    List Vec


type alias Centroids =
    List Vec


type alias Assignment =
    List Int


type ClusterResult
    = ClusterResult
        { centroids : Centroids
        , assignments : Assignment
        , iterations : Int
        }


nearestIndex : List Vec -> Vec -> Int
nearestIndex cents v =
    let
        pairs = cents |> indexedMap (\i c -> ( i, distance c v ))

        -- To find the pair with minimum distance
        minPair =
            case pairs of
                [] ->
                    Nothing
                p :: ps ->
                    Just <| List.foldl
                        (\acc x -> if Tuple.second x < Tuple.second acc then x else acc)
                        p
                        ps
    in
    minPair
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0


assignClusters : List Vec -> DataSet -> Assignment
assignClusters cents dataset =
    dataset |> List.map (nearestIndex cents)


-- Group vectors by cluster index (0..k-1)
groupByCluster : Int -> Assignment -> DataSet -> Dict Int (List Vec)
groupByCluster k assignments dataset =
    let
        pairs = List.map2 Tuple.pair assignments dataset
        folder (c, v) dict =
            let prev = Dict.get c dict |> Maybe.withDefault []
            in
            Dict.insert c (v :: prev) dict
    in
    List.foldl folder Dict.empty pairs


updateCentroids : Int -> Assignment -> DataSet -> Centroids
updateCentroids k assignments dataset =
    let
        groups = groupByCluster k assignments dataset

        defaultVec =
            case dataset of
                [] -> zeros 0
                v :: _ -> zeros (List.length (toList v))

        centroidFor c =
            Dict.get c groups
                |> Maybe.map mean
                |> Maybe.withDefault defaultVec
    in
    List.map centroidFor (List.range 0 (k - 1))


iterateOnce : Int -> List Vec -> DataSet -> ( List Vec, Assignment )
iterateOnce k cents dataset =
    let
        assignments = assignClusters cents dataset
        newCents = updateCentroids k assignments dataset
    in
    ( newCents, assignments )


kMeans : Int -> Int -> DataSet -> ClusterResult
kMeans k maxIter dataset =
    let
        initCentroids =
            dataset |> List.take k

        n = List.length dataset

        loop iter cents prevAssignments =
            if iter >= maxIter then
                ClusterResult { centroids = cents, assignments = prevAssignments, iterations = iter }
            else
                let
                    ( newCents, assignments ) = iterateOnce k cents dataset
                in
                if assignments == prevAssignments then
                    ClusterResult { centroids = newCents, assignments = assignments, iterations = iter }
                else
                    loop (iter + 1) newCents assignments
    in
    if n == 0 then
        ClusterResult { centroids = [], assignments = [], iterations = 0 }
    else
        loop 0 initCentroids (List.repeat n (-1))
