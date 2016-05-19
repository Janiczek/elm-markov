module Markov
    exposing
        ( StateMachine
        , fromInput
        , fromInputs
        , addInput
        , probabilities
        , firstState
        , nextState
        , walk
        )

{-|

# Building a Markov chain
@docs StateMachine, fromInput, fromInputs, addInput

# Inspecting a Markov chain
@docs probabilities

# Generators
@docs firstState, nextState, walk

-}

import Dict exposing (Dict)
import List.Extra as List
import Random.Pcg as Random exposing (Generator)


{-|

    { A: { B: 2
         , C: 10
         , D: 8
         }
    , B: { A: 1 }
    }

-}
type alias Transitions comparable =
    Dict comparable (EndStateCounter comparable)


{-|

    { B: 2
    , C: 10
    , D: 8
    }

-}
type alias EndStateCounter comparable =
    Dict comparable Int


{-|

    [ (0.1, B)
    , (0.5, C)
    , (0.4, D)
    ]

-}
type alias Probabilities comparable =
    List ( Float, comparable )


{-| Intervals to determine (given a random number <0..1>) an end state.

From the example above:

    A -> B: 0.1
    A -> C: 0.6  (= 0.1 + 0.5)
    A -> D: 1.0  (= 0.1 + 0.5 + 0.4)

This means intervals:

    <  0, 0.1>: B
    (0.1, 0.6>: C
    (0.6, 1.0>: D

We represent this as:

    [ (0.1, B)
    , (0.6, C)
    , (1.0, D)
    ]

-}
type alias Intervals comparable =
    List ( Float, comparable )


{-| The `StateMachine` type holds all the needed info for the Markov chain to run.
-}
type alias StateMachine comparable =
    { inputs : List (List comparable)
    , transitions : Transitions comparable
    , intervals : comparable -> Intervals comparable
    }


{-| Create a Markov chain with the given inputs.
-}
fromInputs : List (List comparable) -> StateMachine comparable
fromInputs inputs =
    let
        transitions' =
            transitions inputs

        intervals' =
            intervals transitions'
    in
        { inputs = inputs
        , transitions = transitions'
        , intervals = intervals'
        }


{-| Create a Markov chain with the given input.
-}
fromInput : List comparable -> StateMachine comparable
fromInput input =
    fromInputs [ input ]


{-| Train the Markov chain on some more input.
-}
addInput : List comparable -> StateMachine comparable -> StateMachine comparable
addInput input { inputs } =
    fromInputs (input :: inputs)


transitions : List (List comparable) -> Transitions comparable
transitions inputs =
    let
        restOfInput input =
            List.tail input

        pairs input =
            case restOfInput input of
                Nothing ->
                    []

                Just rest ->
                    List.map2 (,) input rest

        pairs' =
            List.concatMap pairs inputs
    in
        List.foldl addToTransitions Dict.empty pairs'


addToTransitions : ( comparable, comparable ) -> Transitions comparable -> Transitions comparable
addToTransitions ( startState, endState ) transitions =
    let
        incrementEndStateCounter : Maybe Int -> Maybe Int
        incrementEndStateCounter count =
            Just ((Maybe.withDefault 0 count) + 1)

        incrementTransitionCount : Maybe (EndStateCounter comparable) -> Maybe (EndStateCounter comparable)
        incrementTransitionCount maybeCounter =
            let
                counter =
                    Maybe.withDefault Dict.empty maybeCounter

                updatedCounter =
                    Dict.update endState incrementEndStateCounter counter
            in
                Just updatedCounter
    in
        Dict.update startState incrementTransitionCount transitions


{-| Get probabilities for a given starting state.
-}
probabilities : Transitions comparable -> comparable -> Probabilities comparable
probabilities transitions startState =
    let
        transitionsForThisState =
            Dict.get startState transitions
                |> Maybe.withDefault Dict.empty

        totalTransitionCount =
            Dict.size transitionsForThisState

        calcProbability transitionCount =
            (toFloat transitionCount) / (toFloat totalTransitionCount)

        transitionToProbability ( endState, transitionCount ) =
            ( calcProbability transitionCount, endState )
    in
        Dict.toList transitionsForThisState
            |> List.map transitionToProbability


intervals : Transitions comparable -> comparable -> Intervals comparable
intervals transitions startState =
    let
        probabilities' =
            probabilities transitions startState

        ( onlyProbabilities, onlyEndStates ) =
            List.unzip probabilities'

        summedProbabilitiesWithZero =
            List.scanl (+) 0 onlyProbabilities

        maybeSummedProbabilities =
            List.tail summedProbabilitiesWithZero

        summedProbabilities =
            Maybe.withDefault [] maybeSummedProbabilities
    in
        List.map2 (,) summedProbabilities onlyEndStates


{-| Generate a random first state.
-}
firstState : StateMachine comparable -> Generator (Maybe comparable)
firstState stateMachine =
    stateMachine.inputs
        |> List.concat
        |> Random.sample


{-| Generate a random next state given the current state.
-}
nextState : StateMachine comparable -> comparable -> Generator (Maybe comparable)
nextState stateMachine startState =
    let
        intervals' =
            intervals stateMachine.transitions startState

        randomNumberGenerator =
            Random.float 0 1

        getEndState randomNumber =
            Maybe.map snd
                <| List.head
                <| List.dropWhile (\( interval, endState ) -> interval <= randomNumber) intervals'

        endStateGenerator =
            Random.map getEndState
                randomNumberGenerator
    in
        endStateGenerator


{-| Generate a random walk of max length given by the second parameter.
-}
walk : StateMachine comparable -> Int -> Generator (List comparable)
walk stateMachine maxLength =
    let
        generateUpToN : Int -> Maybe comparable -> Generator (List comparable)
        generateUpToN remaining last =
            if remaining == 0 then
                Random.constant []
            else
                case last of
                    Nothing ->
                        Random.constant []

                    Just last' ->
                        let
                            next =
                                (nextState stateMachine last')
                                    `Random.andThen` (generateUpToN (remaining - 1))
                        in
                            next
                                `Random.andThen` (\list -> Random.constant (last' :: list))
    in
        (firstState stateMachine)
            `Random.andThen` (generateUpToN maxLength)
