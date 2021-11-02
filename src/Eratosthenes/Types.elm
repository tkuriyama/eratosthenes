module Eratosthenes.Types exposing (..)

import Dict
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
    , prevMap : Sieve.GeneratorDict WheelState
    , prevNats : List Sieve.Nat
    , newNats : List Sieve.Nat
    }


type alias PrimeGenerator =
    G.Generator Sieve.Nat ( SieveState WheelState, List Sieve.Nat )


type alias SieveState b =
    Sieve.SieveState b


type alias WheelTriple =
    ( List Int
    , Int
    , G.Generator Int WheelState
    )


type alias WheelState =
    ( ( List Int, Int, List Int ), Int )



--------------------------------------------------------------------------------
-- Msg


type Msg
    = WindowResize ( Int, Int )
    | AdvanceSieve Int
    | ResetSieve
    | ChangeWheel WheelTriple
