module Markov
    exposing
        ( Input
        , Transitions
        , EndStateCounter
        , TransitionCount
        , Probabilities
        , Probability
        , transitions
        , addToTransitions
        , probabilities
        )

{-| We want to be able to randomly genererate a number <0..1> and assign a value to it.

If we have a transition table:

    # TRANSITIONS
    A -> B: 2x
    A -> C: 10x
    A -> D: 8x

Then we can see that in the input data we've went from A to something 20 times in total.
We can normalize the counts for given end states into values (percentages) which sum to 1.0:

    # PROBABILITIES
    A -> B: 0.1  (=  2/20)
    A -> C: 0.5  (= 10/20)
    A -> D: 0.4  (=  8/20)

We "lay" these next to each other by summing them:

    # INTERVALS
    A -> B: 0.1
    A -> C: 0.6  (= 0.1 + 0.5)
    A -> D: 1.0  (= 0.1 + 0.5 + 0.4)

And thus form intervals: which random number corresponds to which transition.

After generating a random number <0..1>, we find the first transition that has higher percentage than that number.

    randomNumber = 0.34

    A -> B: too low (0.1 <= 0.34)
    A -> C: enough! (0.6  > 0.34)

Thus we have chosen the transition to state `C`.

# Input types
@docs Input

# Transitions types
@docs Transitions, EndStateCounter, TransitionCount

# Transitions
@docs transitions, addToTransitions

# Probabilities types
@docs Probabilities, Probability

# Probabilities
@docs probabilities

-}

import Dict exposing (Dict)
import Ratio exposing (Rational, over)


{-| Input list from which we generate the Markov chain.

Generally we'll want a string which we'll split and transform, but that's too specific and just one of many possible usages. This is as general as can be.
-}
type alias Input comparable =
    List comparable


{-| How many times did this transition happen?
-}
type alias TransitionCount =
    Int


{-| Probability of the transition.

       how many times did this transition happen
    ----------------------------------------------
    number of all transitions for this start state
-}
type alias Probability =
    Rational


{-| Number representing the end of an interval.

This could still be Rational, but given that we'll compare this to the random numbers a lot, let's save ourselves computing it over and over again.
-}
type alias IntervalEnd =
    Float


{-| All the possible transitions of a state machine.

These are kept in such form that it's easy to extend them after creation.

From the example above:

    A -> B: 2x
    A -> C: 10x
    A -> D: 8x
    B -> A: 1x  -- thrown in for variety

We represent this as (pseudocode):

    { A: { B: 2
         , C: 10
         , D: 8
         }
    , B: { A: 1 }
    }

-}
type alias Transitions comparable =
    Dict comparable (EndStateCounter comparable)


{-| How many times did we get to this state from some start state?
-}
type alias EndStateCounter comparable =
    Dict comparable TransitionCount


{-| Probabilities for a given starting state.

From the example above:

    A -> B: 0.1  (=  2/20)
    A -> C: 0.5  (= 10/20)
    A -> D: 0.4  (=  8/20)

We represent this as:

    [ (Ratio  2 20, B) -- actually all of these get normalized but here we don't care
    , (Ratio 10 20, C)
    , (Ratio  8 20, D)
    ]

-}
type alias Probabilities comparable =
    List ( Probability, comparable )


{-| Intervals for a given starting state.

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
    List ( IntervalEnd, comparable )


{-| Parse the input sequence into a "transition matrix."
-}
transitions : Input comparable -> Transitions comparable
transitions input =
    let
        restOfInput =
            List.tail input

        pairs =
            case restOfInput of
                Nothing ->
                    []

                Just rest ->
                    List.map2 (,) input rest
    in
        List.foldl addToTransitions Dict.empty pairs


{-| Increment a counter for this (startState, endState) pair.
-}
addToTransitions : ( comparable, comparable ) -> Transitions comparable -> Transitions comparable
addToTransitions ( startState, endState ) transitions =
    let
        incrementEndStateCounter : Maybe TransitionCount -> Maybe TransitionCount
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
probabilities stateMachine startState =
    let
        transitionsForThisState =
            Dict.get startState stateMachine
                |> Maybe.withDefault Dict.empty

        totalTransitionCount =
            Dict.size transitionsForThisState

        calcProbability transitionCount =
            transitionCount `over` totalTransitionCount

        transitionToProbability ( endState, transitionCount ) =
            ( calcProbability transitionCount, endState )
    in
        Dict.toList transitionsForThisState
            |> List.map transitionToProbability


{-| Get an interval (see the doc for the type Intervals) for every possibility.
-}
intervals : Probabilities comparable -> Intervals comparable
intervals probabilities =
    let
        ( onlyProbabilities, onlyEndStates ) =
            List.unzip probabilities

        floatProbabilities =
            List.map Ratio.toFloat onlyProbabilities

        summedProbabilitiesWithZero =
            List.scanl (+) 0 floatProbabilities

        maybeSummedProbabilities =
            List.tail summedProbabilitiesWithZero

        summedProbabilities =
            Maybe.withDefault [] maybeSummedProbabilities
    in
        List.map2 (,) summedProbabilities onlyEndStates
