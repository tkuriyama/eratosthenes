module Eratosthenes.View exposing (view)

import Color exposing (Color)
import Dict
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eratosthenes.Types exposing (..)
import Generator as G
import Html exposing (Html)



-- import Html.Attributes as Attr exposing (id)
--------------------------------------------------------------------------------


view : Model -> Html msg
view model =
    E.layout
        [ E.width (E.px model.windowWidth)
        , Font.family [ Font.typeface "Consolas", Font.sansSerif, Font.monospace ]
        , Font.size 18
        , E.padding 5
        ]
        (E.column
            [ E.width E.fill
            , E.alignLeft
            ]
            [ controlView
            , nextView model.sieve
            , E.row
                [ E.width E.fill ]
                [ E.el
                    [ E.width <| E.fillPortion 1 ]
                    (natsView model.nats)
                , E.el
                    [ E.width <| E.fillPortion 1 ]
                    (mapView <| G.inspect model.sieve)
                ]
            ]
        )


controlView : E.Element msg
controlView =
    E.none


nextView : PrimeGenerator -> E.Element msg
nextView generator =
    E.none


natsView : List Nat -> E.Element msg
natsView ints =
    E.none


mapView : Maybe ( SieveState WheelState, List Nat ) -> E.Element msg
mapView map =
    E.none
