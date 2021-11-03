module Eratosthenes.View exposing (view)

import Dict
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eratosthenes.Sieve as Sieve exposing (Nat(..))
import Eratosthenes.Types exposing (..)
import Eratosthenes.Utils as Utils
import Generator as G
import Html exposing (Html)



--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    E.layout
        [ E.width (E.px model.windowWidth)
        , Font.family [ Font.typeface "Consolas", Font.sansSerif, Font.monospace ]
        , Font.size 18
        , E.paddingXY 60 20
        ]
        (E.column
            [ E.width E.fill
            , E.alignLeft
            ]
            [ titleView
            , noteView
            , controlView model.wheel
            , columnTitleView model.sieve
            , E.row
                [ E.width E.fill
                , E.padding 10
                , E.spacing 20
                ]
                [ E.el
                    [ E.width <| E.fillPortion 1
                    , E.alignTop
                    , E.height E.fill
                    ]
                    (natsView model.prevNats model.newNats)
                , E.el
                    [ E.width <| E.fillPortion 1
                    , E.alignTop
                    ]
                    (mapView
                        (Utils.getMap model.sieve)
                        model.prevMap
                    )
                ]
            ]
        )


columnTitleView : PrimeGenerator -> E.Element msg
columnTitleView sieve =
    E.row
        [ E.width E.fill
        , E.paddingEach { top = 20, bottom = 5, left = 10, right = 10 }
        , E.spacing 20
        , Font.size 20
        ]
        [ E.paragraph
            [ E.width <| E.fillPortion 1 ]
            [ E.el
                [ Font.bold ]
                (E.text <| "Verified Naturals")
            , E.el []
                (E.text <| nextUp sieve)
            ]
        , E.el
            [ E.width <| E.fillPortion 1
            , Font.bold
            ]
            (E.text "Map of Composites")
        ]


nextUp : PrimeGenerator -> String
nextUp generator =
    case G.head generator of
        Nothing ->
            ""

        Just n ->
            " (Next Candidate: " ++ String.fromInt (natToInt n) ++ ")"


natToInt : Sieve.Nat -> Int
natToInt n =
    case n of
        Prime p ->
            p

        Composite c ->
            c



--------------------------------------------------------------------------------


titleView : E.Element msg
titleView =
    E.row
        [ E.centerX ]
        [ E.el
            [ Font.size 24
            ]
            (E.el
                [ E.onRight links
                ]
                (E.text "Sieve of Eratosthenes")
            )
        ]


links : E.Element msg
links =
    E.row
        [ Font.size 16
        , E.paddingXY 30 0
        , E.alignBottom
        , E.spacing 10
        , Font.color <| E.rgb255 173 216 230
        ]
        [ E.link
            [ Font.underline ]
            { url = "https://tkuriyama.github.io/general/2021/10/30/eratosthenes-elm-generator.html"
            , label = E.text "About"
            }
        , E.link
            [ Font.underline ]
            { url = "https://github.com/tkuriyama/eratosthenes"
            , label = E.text "Code"
            }
        ]


noteView : E.Element msg
noteView =
    E.row
        [ E.centerX
        , E.paddingEach { top = 5, bottom = 10, right = 0, left = 0 }
        ]
        [ E.paragraph
            [ Font.size 14 ]
            [ E.text "Visualizing the step-by-step progress of an efficient, iterative algorithm for the sieve."
            ]
        ]



--------------------------------------------------------------------------------


controlView : WheelTriple -> E.Element Msg
controlView selectedWheel =
    E.row
        [ E.padding 10
        , E.spacing 10
        , E.centerX
        ]
        [ clearButton
        , advanceButton 1 "Advance 1"
        , advanceButton 10 "Advance 10"
        , wheels selectedWheel
        ]


wheels : WheelTriple -> E.Element Msg
wheels selected =
    choice
        ""
        ChangeWheel
        [ Input.option Sieve.wheel2Init <| E.text "Wheel 2"
        , Input.option Sieve.wheel2357Init <| E.text "Wheel 2357"
        ]
        selected


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


mapView :
    Sieve.GeneratorDict WheelState
    -> Sieve.GeneratorDict WheelState
    -> E.Element msg
mapView map prevMap =
    E.column
        [ E.width E.fill
        ]
        (tableHeaders
            :: List.map (showRow prevMap) (Dict.toList map)
        )


tableHeaders : E.Element msg
tableHeaders =
    E.row
        [ E.width E.fill
        ]
        [ E.el
            (cellStyle 1 (Just <| E.rgb255 220 220 220))
            (E.text "Composite")
        , E.el
            (cellStyle 6 (Just <| E.rgb255 220 220 220))
            (E.text "Prime x Sieve Multiplier")
        ]


showRow :
    Sieve.GeneratorDict WheelState
    -> ( Int, List (G.Generator Int WheelState) )
    -> E.Element msg
showRow prevMap ( key, generators ) =
    let
        ( keyBackground, valueBackground ) =
            case Dict.member key prevMap of
                True ->
                    let
                        oldCount =
                            Dict.get key prevMap
                                |> Maybe.map List.length
                                |> Maybe.withDefault 0
                    in
                    case List.length generators == oldCount of
                        True ->
                            ( Nothing, Nothing )

                        False ->
                            ( Nothing, Just highlightColor )

                False ->
                    ( Just highlightColor, Just highlightColor )
    in
    E.row
        [ E.width E.fill ]
        [ E.el
            (cellStyle 1 keyBackground)
            (E.text <| String.fromInt key)
        , E.el
            (cellStyle 6 valueBackground)
            (E.text <| getFactors key generators)
        ]


getFactors : Int -> List (G.Generator Int WheelState) -> String
getFactors composite =
    List.map (getFactor composite) >> List.intersperse ", " >> String.concat


getFactor : Int -> G.Generator Int WheelState -> String
getFactor composite =
    G.inspect
        >> Maybe.map (\( ( _, _, _ ), x ) -> x)
        >> Maybe.withDefault 0
        >> showFactors composite


showFactors : Int -> Int -> String
showFactors composite n =
    let
        source =
            composite // n |> String.fromInt

        multiplier =
            n |> String.fromInt
    in
    source ++ " x " ++ multiplier


cellStyle : Int -> Maybe E.Color -> List (E.Attribute msg)
cellStyle fill bgColor =
    let
        bg =
            case bgColor of
                Just color ->
                    [ Background.color color ]

                Nothing ->
                    []
    in
    [ E.width <| E.fillPortion fill
    , E.paddingXY 5 3
    , Border.width 1
    , Border.color <| E.rgb255 125 125 125
    , Border.dotted
    ]
        ++ bg



--------------------------------------------------------------------------------
-- helpers


highlightColor : E.Color
highlightColor =
    E.rgb255 172 250 47


choice : String -> (a -> msg) -> List (Input.Option a msg) -> a -> E.Element msg
choice title msg options selected =
    Input.radioRow
        [ E.padding 10
        , E.spacing 10
        ]
        { onChange = msg
        , selected = Just selected
        , label = titleLabel title
        , options = options
        }


titleLabel : String -> Input.Label msg
titleLabel s =
    Input.labelLeft
        [ Font.heavy
        , E.centerX
        ]
        (E.text s)


titleLabelLight : String -> Input.Label msg
titleLabelLight s =
    Input.labelLeft
        [ E.centerX
        ]
        (E.text s)
