module Eratosthenes.View exposing (view)

import Dict
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eratosthenes.Sieve exposing (Nat(..))
import Eratosthenes.Types exposing (..)
import Generator as G
import Html exposing (Html)



--------------------------------------------------------------------------------


view : Model -> Html Msg
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
            [ titleView
            , noteView
            , controlView
            , nextView model.sieve
            , E.row
                [ E.width E.fill
                , E.padding 10
                , E.spacing 20
                ]
                [ E.el
                    [ E.width <| E.fillPortion 1 ]
                    (natsView model.prevNats model.newNats)
                , E.el
                    [ E.width <| E.fillPortion 1 ]
                    (mapView <| G.inspect model.sieve)
                ]
            ]
        )



--------------------------------------------------------------------------------


titleView : E.Element msg
titleView =
    E.row
        [ E.centerX ]
        [ E.el
            [ Font.size 24
            ]
            (E.text "Sieve of Eratosthenes")
        ]


noteView : E.Element msg
noteView =
    E.row
        [ E.centerX
        , E.paddingEach { top = 5, bottom = 10, right = 0, left = 0 }
        ]
        [ E.el
            [ Font.size 14 ]
            (E.text "Visualizing the step-by-step progress of an iterative algorithm for the sieve. Detailed explanation here.")
        ]



--------------------------------------------------------------------------------


controlView : E.Element Msg
controlView =
    E.row
        [ E.padding 10
        , E.spacing 10
        ]
        [ clearButton
        , advanceButton 1 "Advance 1"
        , advanceButton 10 "Advance 10"
        ]


clearButton : E.Element Msg
clearButton =
    Input.button
        [ E.paddingXY 5 2
        , Border.rounded 5
        , Border.width 1
        , Background.color <| E.rgb255 250 186 186
        ]
        { onPress = Just <| ResetSieve
        , label = E.text "Reset"
        }


advanceButton : Int -> String -> E.Element Msg
advanceButton n title =
    Input.button
        [ E.paddingXY 5 2
        , Border.rounded 5
        , Border.width 1

        --, Background.color <| E.rgb255 250 186 186
        ]
        { onPress = Just <| AdvanceSieve n
        , label = E.text title
        }



--------------------------------------------------------------------------------


nextView : PrimeGenerator -> E.Element msg
nextView generator =
    E.none



--------------------------------------------------------------------------------


natsView : List Nat -> List Nat -> E.Element msg
natsView prevNats newNats =
    E.wrappedRow
        [ E.padding 10
        , E.spacing 5
        ]
        (List.map (showNat False) prevNats
            ++ List.map (showNat True) newNats
        )


showNat : Bool -> Nat -> E.Element msg
showNat new n =
    let
        baseStyle =
            [ E.paddingXY 2 1
            , Border.rounded 3
            ]

        style =
            case new of
                True ->
                    Background.color highlightColor :: baseStyle

                False ->
                    baseStyle
    in
    case n of
        Prime p ->
            E.el
                style
                (E.text <| String.fromInt p)

        Composite c ->
            E.el
                (Font.strike :: style)
                (E.text <| String.fromInt c)



--------------------------------------------------------------------------------


mapView : Maybe ( SieveState WheelState, List Nat ) -> E.Element msg
mapView map =
    E.none



--------------------------------------------------------------------------------


highlightColor : E.Color
highlightColor =
    E.rgb255 172 250 47
