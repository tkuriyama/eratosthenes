module Eratosthenes.Utils exposing (..)

import Dict
import Eratosthenes.Sieve as Sieve
import Eratosthenes.Types exposing (..)
import Generator as G



--------------------------------------------------------------------------------


getMap : PrimeGenerator -> Sieve.GeneratorDict WheelState
getMap generator =
    let
        map =
            G.inspect generator
    in
    case map of
        Nothing ->
            Dict.empty

        Just state ->
            Tuple.first state |> Tuple.first
