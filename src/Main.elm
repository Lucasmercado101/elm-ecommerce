module Main exposing (..)

import Browser
import Html exposing (Html, div, text)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { count : Int }


init : Model
init =
    { count = 0 }



-- UPDATE


type Msg
    = Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "a" ]
