module Eratosthenes.View exposing (view)

import Color exposing (Color)
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eratosthenes.Types exposing (..)
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
            []
        )
