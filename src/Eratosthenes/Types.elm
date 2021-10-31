module Eratosthenes.Types exposing (..)

import Eratosthenes.Sieve as Sieve
import Generator as G



--------------------------------------------------------------------------------
-- Flats, Model


type alias Flags =
    { windowWidth : Float
    , windowHeight : Float
    }


type alias Model =
    { windowWidth : Int
    , windowHeight : Int
    , wheel : WheelTriple
    , sieve : PrimeGenerator
    , primes : List Int
    }


type alias PrimeGenerator =
    G.Generator
        Int
        ( Sieve.SieveState ( ( List Int, Int, List Int ), Int ), List Int )


type alias WheelTriple =
    ( List Int
    , Int
    , G.Generator Int ( ( List Int, Int, List Int ), Int )
    )



--------------------------------------------------------------------------------
-- Msg


type Msg
    = WindowResize ( Int, Int )
    | AdvanceSieve Int
    | ResetSieve
    | ChangeWheel WheelTriple
