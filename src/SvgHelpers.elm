module SvgHelpers exposing
    ( Domain
    , viewCanvas
    , viewPoint
    , line
    , textAt
    , toSvgX
    , toSvgY
    )

import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Domain =
    { xMin : Float
    , xMax : Float
    , yMin : Float
    , yMax : Float
    }


toSvgX : Domain -> Float -> Float -> Float
toSvgX domain width x =
    let
        span = domain.xMax - domain.xMin
        t =
            if span == 0 then
                0
            else
                (x - domain.xMin) / span
    in
    t * width


toSvgY : Domain -> Float -> Float -> Float
toSvgY domain height y =
    let
        span = domain.yMax - domain.yMin
        t =
            if span == 0 then
                0
            else
                (y - domain.yMin) / span
    in
    height - (t * height)



viewPoint : Domain -> Float -> Float -> ( Float, Float ) -> String -> Svg msg
viewPoint domain w h ( ux, uy ) color =
    circle
        [ cx (String.fromFloat (toSvgX domain w ux))
        , cy (String.fromFloat (toSvgY domain h uy))
        , r "5"
        , fill color
        , stroke "black"
        , strokeWidth "0.5"
        ]
        []



line :
    Domain
    -> Float
    -> Float
    -> ( Float, Float )
    -> ( Float, Float )
    -> String
    -> Svg msg
line domain w h ( x1Val, y1Val ) ( x2Val, y2Val ) color =
    Svg.line
        [ Svg.Attributes.x1 (String.fromFloat (toSvgX domain w x1Val))
        , Svg.Attributes.y1 (String.fromFloat (toSvgY domain h y1Val))
        , Svg.Attributes.x2 (String.fromFloat (toSvgX domain w x2Val))
        , Svg.Attributes.y2 (String.fromFloat (toSvgY domain h y2Val))
        , stroke color
        , strokeWidth "2"
        ]
        []


textAt :
    Domain
    -> Float
    -> Float
    -> ( Float, Float )
    -> String
    -> Svg msg
textAt domain w h ( ux, uy ) txt =
    Svg.text_
        [ x (String.fromFloat (toSvgX domain w ux))
        , y (String.fromFloat (toSvgY domain h uy))
        , fontSize "12"
        , fill "black"
        ]
        [ Svg.text txt ]


viewCanvas : Float -> Float -> List (Svg msg) -> Svg msg
viewCanvas w h children =
    svg
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox ("0 0 " ++ String.fromFloat w ++ " " ++ String.fromFloat h)
        ]
        children
