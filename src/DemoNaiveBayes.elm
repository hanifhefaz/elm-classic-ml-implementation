module DemoNaiveBayes exposing (Model, Msg, init, update, view)


-- Naive Bayes text demo: train from Dataset.spamSmall and classify user input.
import Html exposing (Html, div, input, button, text)
import Html.Attributes exposing (value, placeholder)
import Html.Events exposing (onInput, onClick)
import NaiveBayes
import Dataset


type alias Model =
    { model : NaiveBayes.Model
    , query : String
    , prediction : Maybe ( String, Float )
    }


type Msg
    = SetQuery String
    | Classify
    | Reset


init : Model
init =
    let
        ds = Dataset.spamSmall
        m = NaiveBayes.fit 1.0 ds
    in
    { model = m, query = "", prediction = Nothing }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetQuery s ->
            { model | query = s }

        Classify ->
            let
                pred = NaiveBayes.predict model.model model.query
            in
            { model | prediction = Just pred }

        Reset ->
            { model | query = "", prediction = Nothing }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ placeholder "Type sentence...", value model.query, onInput SetQuery ] []
            , button [ onClick Classify ] [ text "Classify" ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]
        , div [] (
            case model.prediction of
                Nothing -> [ text "Prediction: -" ]
                Just (lab,score) -> [ text ("Prediction: " ++ lab ++ " (score: " ++ String.fromFloat score ++ ")") ]
          )
        ]
