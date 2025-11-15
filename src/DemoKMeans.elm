module DemoKMeans exposing (Model, Msg, init, update, view)


import Html exposing (Html, div, button, text, input, label)
import Html.Attributes exposing (type_, value, step)
import Html.Events exposing (onClick, onInput)
import Svg exposing (Svg)
import SvgHelpers exposing (viewCanvas, viewPoint, line, textAt)
import Vector exposing (Vec, toList, fromList)
import KMeans
import List exposing (map)
import Dataset exposing (irisSmall, toNumericIris)


type alias Model =
    { k : Int
    , maxIter : Int
    , dataset : List Vec
    , result : Maybe KMeans.ClusterResult
    , domain : { xMin:Float, xMax:Float, yMin:Float, yMax:Float }
    }


type Msg
    = SetK String
    | SetMaxIter String
    | Run
    | Reset


init : Model
init =
    let
        ds = Dataset.toNumericIris Dataset.irisSmall |> List.map Tuple.first
        xsList = List.concatMap toList ds
        xsOnly = List.map Tuple.first (List.map (\v -> ( List.head (toList v) |> Maybe.withDefault 0.0, 0.0)) ds) -- placeholder
        domain =
            { xMin = 0, xMax = 8, yMin = 0, yMax = 6 }
    in
    { k = 2, maxIter = 10, dataset = ds, result = Nothing, domain = domain }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetK s ->
            case String.toInt s of
                Just n -> { model | k = max 1 n }
                Nothing -> model

        SetMaxIter s ->
            case String.toInt s of
                Just n -> { model | maxIter = max 1 n }
                Nothing -> model

        Run ->
            let
                res = KMeans.kMeans model.k model.maxIter model.dataset
            in
            { model | result = Just res }

        Reset ->
            { model | result = Nothing }


-- convert Vec -> (x,y)
vecToXY : Vec -> ( Float, Float )
vecToXY v =
    case toList v of
        x :: y :: _ -> ( x, y )
        [ x ] -> ( x, 0 )
        _ -> ( 0, 0 )


view : Model -> Html Msg
view model =
    let
        svgWidth = 600
        svgHeight = 400
        children =
            case model.result of
                Nothing ->
                    -- draw only points
                    List.map (\v -> viewPoint model.domain svgWidth svgHeight (vecToXY v) "steelblue") model.dataset

                Just (KMeans.ClusterResult r) ->
                    let
                        pts =
                            List.map (\v -> viewPoint model.domain svgWidth svgHeight (vecToXY v) "lightsteelblue") model.dataset

                        cents =
                            List.map (\c -> viewPoint model.domain svgWidth svgHeight (vecToXY c) "orange") r.centroids
                    in
                    pts ++ cents
    in
    div []
        [ div []
            [ label [] [ text "k: " ]
            , input [ type_ "range", Html.Attributes.min "1", Html.Attributes.max "6", step "1", value (String.fromInt model.k), onInput SetK ] []
            , label [] [ text ("  maxIter: ") ]
            , input [ type_ "number", value (String.fromInt model.maxIter), onInput SetMaxIter ] []
            , button [ onClick Run ] [ text "Run k-Means" ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]
        , div [] [ viewCanvas svgWidth svgHeight children ]
        ]
