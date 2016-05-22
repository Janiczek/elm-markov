module Main exposing (..)

import Dict exposing (Dict)
import Html as H exposing (Html, Attribute)
import Html.Attributes as HA
import Html.Events as HE
import Html.App as H
import Markov exposing (StateMachine)
import Random.Pcg as Random
import String


main : Program Flags
main =
    H.programWithFlags
        { init = (\flags -> (initModel flags) ! [])
        , update = (\msg model -> (update msg model) ! [])
        , view = view
        , subscriptions = always Sub.none
        }


type alias Flags =
    { seed1 : Int
    , seed2 : Int
    }


type alias Model =
    { inputs : Dict Int String
    , length : Int
    , generatedString : String
    , newId : Int
    , randomSeed : Random.Seed
    }


initModel : Flags -> Model
initModel { seed1, seed2 } =
    { inputs = Dict.singleton 0 ""
    , length = 30
    , generatedString = ""
    , newId = 1
    , randomSeed = Random.initialSeed2 seed1 seed2
    }


type Msg
    = GenerateString
    | UpdateLength String
    | NewInput
    | UpdateInput Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        GenerateString ->
            let
                stateMachine =
                    model.inputs
                        |> Dict.values
                        |> List.map sanitizeInput
                        |> Markov.fromInputs

                ( generatedList, randomSeed ) =
                    Random.step (Markov.walk stateMachine model.length) model.randomSeed

                generatedString =
                    String.join " " generatedList
            in
                { model
                    | randomSeed = randomSeed
                    , generatedString = generatedString
                }

        UpdateLength stringLength ->
            let
                length =
                    String.toInt stringLength
                        |> Result.withDefault model.length
            in
                { model | length = length }

        NewInput ->
            { model
                | inputs = Dict.insert model.newId "" model.inputs
                , newId = model.newId + 1
            }

        UpdateInput index contents ->
            { model | inputs = Dict.insert index contents model.inputs }


sanitizeInput : String -> List String
sanitizeInput input =
    input
        |> String.trim
        |> String.split " "


view : Model -> Html Msg
view model =
    H.div []
        [ H.h1 [] [ H.text "elm-markov" ]
        , H.p [] [ H.text "Train the Markov chain and see what it comes up with!" ]
        , H.div [ HA.class "container" ]
            [ H.div [ HA.class "row" ]
                [ leftColumn model
                , middleColumn model
                , rightColumn model
                ]
            ]
        ]


leftColumn : Model -> Html Msg
leftColumn model =
    column
        [ formGroup
            [ H.h2 [] [ H.text "Inputs" ]
            , button [ HE.onClick NewInput ] "Add input"
            , H.div []
                (List.map viewInput (Dict.toList model.inputs))
            ]
        ]


viewInput : ( Int, String ) -> Html Msg
viewInput ( id, input ) =
    H.div []
        [ H.h3 [] [ H.text ("Input " ++ toString id) ]
        , textarea
            [ HA.rows 3
            , HE.onInput (UpdateInput id)
            ]
            input
        ]


middleColumn : Model -> Html Msg
middleColumn model =
    column
        [ formGroup
            [ label "Length (in words)"
            , input
                [ HA.type' "number"
                , HE.onInput UpdateLength
                , HA.value (toString model.length)
                ]
            ]
        , button [ HE.onClick GenerateString ] "Generate!"
        ]


rightColumn : Model -> Html Msg
rightColumn model =
    column
        [ H.h2 [] [ H.text "Generated string" ]
        , textarea [ HA.rows 5, HA.disabled True ] model.generatedString
        ]


column : List (Html Msg) -> Html Msg
column =
    H.div
        [ HA.class "col-md-4"
        , HA.style
            [ ( "float", "left" )
            , ( "width", "33%" )
            ]
        ]


formGroup : List (Html Msg) -> Html Msg
formGroup =
    H.div [ HA.class "form-group" ]


button : List (Attribute Msg) -> String -> Html Msg
button attributes string =
    H.button (List.append attributes [ HA.type' "button", HA.class "btn btn-primary" ])
        [ H.text string ]


label : String -> Html Msg
label string =
    H.label [] [ H.text string ]


input : List (Attribute Msg) -> Html Msg
input attributes =
    H.input (List.append attributes [ HA.class "form-control" ]) []


textarea : List (Attribute Msg) -> String -> Html Msg
textarea attributes string =
    H.textarea (List.append attributes [ HA.class "form-control" ])
        [ H.text string ]
