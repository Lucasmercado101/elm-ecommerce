module Icons.FavoriteOutline exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


favoriteOutline : Float -> Float -> String -> Html msg
favoriteOutline w h color =
    svg
        [ w
            |> String.fromFloat
            |> width
        , h
            |> String.fromFloat
            |> height
        , viewBox "0 0 16 16"
        , fill "none"
        ]
        [ g []
            [ Svg.path
                [ d "M11 2C9.84001 2 8.72668 2.54 8.00001 3.39333C7.27334 2.54 6.16001 2 5.00001 2C2.94668 2 1.33334 3.61333 1.33334 5.66667C1.33334 8.18667 3.60001 10.24 7.03334 13.36L8.00001 14.2333L8.96668 13.3533C12.4 10.24 14.6667 8.18667 14.6667 5.66667C14.6667 3.61333 13.0533 2 11 2ZM8.06668 12.3667L8.00001 12.4333L7.93334 12.3667C4.76001 9.49333 2.66668 7.59333 2.66668 5.66667C2.66668 4.33333 3.66668 3.33333 5.00001 3.33333C6.02668 3.33333 7.02668 3.99333 7.38001 4.90667H8.62668C8.97334 3.99333 9.97334 3.33333 11 3.33333C12.3333 3.33333 13.3333 4.33333 13.3333 5.66667C13.3333 7.59333 11.24 9.49333 8.06668 12.3667Z"
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
