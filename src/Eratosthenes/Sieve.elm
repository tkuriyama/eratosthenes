module Eratosthenes.Sieve exposing (..)

{-| Sieve of Eratosthenes examples.

See <https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf> for further details on the algorithms.

-}

import Dict
import Generator as G



--------------------------------------------------------------------------------


type Nat
    = Prime Int
    | Composite Int



---
--------------------------------------------------------------------------------


{-| Primes by an incremental Sieve of Eratosthenes.

The idea is to store the sieve's composite generators in a Dict, and update them just-in-time as more and more candidates are explored.

    import Generator as G

    wheel2Init |> sieve
    |> G.take 5
    --> [ Prime 2, Prime 3, Prime 5, Prime 7, Composite 9]

    wheel2357Init |> sieve
    |> G.take 10
     --> [ Prime 2, Prime 3, Prime 5, Prime 7, Prime 11, Prime 13, Prime 17, Prime 19, Prime 23, Prime 29 ]

See section 3 of <https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>

-}
type alias SieveState b =
    ( GeneratorDict b, G.Generator Int b )


type alias GeneratorDict b =
    Dict.Dict Int (List (G.Generator Int b))


sieve :
    ( List Int, Int, G.Generator Int b )
    -> G.Generator Nat ( SieveState b, List Nat )
sieve ( primes, lastPrime, candidateWheel ) =
    let
        state0 =
            ( insertNext lastPrime candidateWheel Dict.empty
            , candidateWheel
            )
    in
    G.prefix (List.map Prime primes) <| G.init state0 sieveNext


sieveNext : SieveState b -> Maybe ( Nat, SieveState b )
sieveNext ( map, wheel ) =
    let
        ( guess, wheel_ ) =
            safeAdvance1 wheel
    in
    case Dict.get guess map of
        Nothing ->
            Just
                ( Prime guess, ( insertNext guess wheel_ map, wheel_ ) )

        Just composites ->
            Just
                ( Composite guess, ( updateMap guess composites map, wheel_ ) )


insertNext : Int -> G.Generator Int b -> GeneratorDict b -> GeneratorDict b
insertNext n generator =
    Dict.insert
        (n * n)
        [ G.map ((*) n) generator ]


updateMap :
    Int
    -> List (G.Generator Int b)
    -> GeneratorDict b
    -> GeneratorDict b
updateMap guess compositeGenerators =
    let
        reinsert compositeGenerator map_ =
            safeAdvance1 compositeGenerator |> insertToList map_
    in
    Dict.remove guess
        >> (\map -> List.foldl reinsert map compositeGenerators)



--------------------------------------------------------------------------------
-- Seeds / Candidates


wheel2 =
    G.cycle [ 2 ]


wheel2Init =
    ( [ 2, 3 ], 3, wheel2 |> G.scanl (+) 3 )


wheel2357 =
    G.cycle [ 2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6, 2, 6, 4, 2, 6, 4, 6, 8, 4, 2, 4, 2, 4, 8, 6, 4, 6, 2, 4, 6, 2, 6, 6, 4, 2, 4, 6, 2, 6, 4, 2, 4, 2, 10, 2, 10 ]


wheel2357Init =
    ( [ 2, 3, 5, 7, 11 ], 11, wheel2357 |> G.scanl (+) 11 )



--------------------------------------------------------------------------------
-- Helpers


insertToList :
    Dict.Dict comparable (List v)
    -> ( comparable, v )
    -> Dict.Dict comparable (List v)
insertToList map ( key, val ) =
    case Dict.member key map of
        False ->
            Dict.insert key [ val ] map

        True ->
            Dict.update key (Maybe.map ((::) val)) map


safeAdvance1 : G.Generator Int b -> ( Int, G.Generator Int b )
safeAdvance1 =
    let
        unwrap =
            List.head >> Maybe.withDefault 0
    in
    G.advance 1 >> Tuple.mapFirst unwrap
