module Icons.FavoriteFilled exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


favoriteFilled : Float -> Float -> String -> Html msg
favoriteFilled w h color =
    svg
        [ w
            |> String.fromFloat
            |> width
        , h
            |> String.fromFloat
            |> height
        , viewBox "0 0 18 18"
        , fill "none"
        ]
        [ g []
            [ Svg.path
                [ d "M9 16.0125L7.9125 15.0225C4.05 11.52 1.5 9.21 1.5 6.375C1.5 4.065 3.315 2.25 5.625 2.25C6.93 2.25 8.1825 2.8575 9 3.8175C9.8175 2.8575 11.07 2.25 12.375 2.25C14.685 2.25 16.5 4.065 16.5 6.375C16.5 9.21 13.95 11.52 10.0875 15.03L9 16.0125Z"
                , fill color
                ]
                []
            ]
        , defs []
            [ rect
                [ w
                    |> String.fromFloat
                    |> width
                , h
                    |> String.fromFloat
                    |> height
                , fill color
                ]
                []
            ]
        ]
