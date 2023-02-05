module Icons.Add exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


add : Float -> Float -> Html msg
add w h =
    Svg.svg
        [ w
            |> String.fromFloat
            |> width
        , h
            |> String.fromFloat
            |> height
        , viewBox "0 0 96 96"
        , fill "none"
        ]
        [ g []
            [ Svg.path
                [ d "M76 52H52V76H44V52H20V44H44V20H52V44H76V52Z"
                , fill "black"
                ]
                []
            ]
        , defs []
            [ Svg.clipPath [ id "clip0_1_162" ]
                [ rect
                    [ w
                        |> String.fromFloat
                        |> width
                    , h
                        |> String.fromFloat
                        |> height
                    , fill "white"
                    ]
                    []
                ]
            ]
        ]
