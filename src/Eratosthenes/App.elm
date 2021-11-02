module Eratosthenes.App exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Dict
import Eratosthenes.Sieve as Sieve
import Eratosthenes.Types exposing (..)
import Eratosthenes.Utils as Utils
import Eratosthenes.View as View
import Generator as G



--------------------------------------------------------------------------------
-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }



--------------------------------------------------------------------------------
-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        windowW =
            flags.windowWidth

        windowH =
            flags.windowHeight
    in
    ( { windowWidth = round windowW
      , windowHeight = round windowH
      , wheel = Sieve.wheel2357Init
      , sieve = Sieve.sieve Sieve.wheel2357Init
      , prevMap = Dict.empty
      , prevNats = []
      , newNats = []
      }
    , Cmd.none
    )



--------------------------------------------------------------------------------
-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize ( w, h ) ->
            ( { model | windowWidth = w, windowHeight = h }
            , Cmd.none
            )

        AdvanceSieve n ->
            ( advanceSieve n model, Cmd.none )

        ResetSieve ->
            ( { model
                | sieve = Sieve.sieve model.wheel
                , prevMap = Dict.empty
                , prevNats = []
                , newNats = []
              }
            , Cmd.none
            )

        ChangeWheel wheel ->
            ( { model
                | sieve = Sieve.sieve wheel
                , wheel = wheel
                , prevMap = Dict.empty
                , prevNats = []
                , newNats = []
              }
            , Cmd.none
            )


advanceSieve : Int -> Model -> Model
advanceSieve n model =
    let
        ( ns, sieve_ ) =
            G.advance n model.sieve
    in
    { model
        | sieve = sieve_
        , prevMap = Utils.getMap model.sieve
        , prevNats = model.prevNats ++ model.newNats
        , newNats = ns
    }



--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\w h -> WindowResize ( w, h ))
        ]
