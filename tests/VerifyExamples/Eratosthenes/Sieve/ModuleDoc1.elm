module VerifyExamples.Eratosthenes.Sieve.ModuleDoc1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Eratosthenes.Sieve exposing (..)
import Generator as G







spec1 : Test.Test
spec1 =
    Test.test "Module VerifyExamples: \n\n    wheel2Init |> sieve\n    |> G.take 10\n    --> [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]" <|
        \() ->
            Expect.equal
                (
                wheel2Init |> sieve
                |> G.take 10
                )
                (
                [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]
                )