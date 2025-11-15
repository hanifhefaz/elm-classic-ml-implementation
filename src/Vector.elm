module Vector exposing
  ( Vec
  , fromList
  , toList
  , add
  , sub
  , dot
  , scalarMul
  , norm
  , distance
  , zeros
  , mean
  )

{-|
Basic vector utilities using Elm Arrays (pure and functional).
-}

import Array exposing (Array)
import List exposing (foldl)


type alias Vec = Array Float


fromList : List Float -> Vec
fromList = Array.fromList


toList : Vec -> List Float
toList = Array.toList


zeros : Int -> Vec
zeros n =
    Array.repeat n 0.0


map2 : (Float -> Float -> Float) -> Vec -> Vec -> Vec
map2 f a b =
    let
        la = toList a
        lb = toList b
        zipped = List.map2 f la lb
    in
    Array.fromList zipped


map1 : (Float -> Float) -> Vec -> Vec
map1 f v =
    v |> toList |> List.map f |> Array.fromList


add : Vec -> Vec -> Vec
add = map2 (+)


sub : Vec -> Vec -> Vec
sub = map2 (-)


dot : Vec -> Vec -> Float
dot a b =
    List.map2 (*) (toList a) (toList b)
        |> foldl (+) 0.0


scalarMul : Float -> Vec -> Vec
scalarMul s v =
    map1 (\x -> s * x) v


norm : Vec -> Float
norm v =
    v
        |> toList
        |> List.map (\x -> x * x)
        |> foldl (+) 0.0
        |> sqrt


distance : Vec -> Vec -> Float
distance a b =
    sub a b |> norm


mean : List Vec -> Vec
mean vecs =
    case vecs of
        [] ->
            Array.fromList []

        v :: _ ->
            let
                n = List.length vecs |> toFloat
                init = zeros (Array.length v)
                sumVec =
                    List.foldl (\x acc -> add x acc) init vecs
            in
            scalarMul (1 / n) sumVec
