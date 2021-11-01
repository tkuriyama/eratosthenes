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
    , compositeMap : Dict.Dict Int WheelState
    , nats : List Nat
    }


type alias PrimeGenerator =
    G.Generator Nat ( Sieve.SieveState WheelState, List Nat )


type alias WheelTriple =
    ( List Int
    , Int
    , G.Generator Int WheelState
    )


type alias WheelState =
    ( ( List Int, Int, List Int ), Int )


type alias Nat =
    Sieve.Nat



--------------------------------------------------------------------------------
-- Msg


type Msg
    = WindowResize ( Int, Int )
    | AdvanceSieve Int
    | ResetSieve
    | ChangeWheel WheelTriple
