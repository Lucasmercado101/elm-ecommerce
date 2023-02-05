module Main exposing (..)

import Api exposing (createProduct, getAllProducts)
import Browser
import Html exposing (Html, button, div, input, label, text, textarea)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import List.Nonempty as NEList exposing (Nonempty(..))
import String exposing (toFloat)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type ApiCall a b
    = Fetching
    | Fetched b
    | Failed a


fromResult : Result a e -> ApiCall a e
fromResult result =
    case result of
        Ok a ->
            Fetched a

        Err e ->
            Failed e


mapFetched : (a -> b) -> ApiCall e a -> ApiCall e b
mapFetched f apiCall =
    case apiCall of
        Fetching ->
            Fetching

        Fetched a ->
            Fetched (f a)

        Failed e ->
            Failed e


type alias Model =
    { name : String
    , price : String
    , products : ApiCall Http.Error (Maybe (Nonempty Api.Product))
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { name = ""
      , price = ""
      , products = Fetching
      }
    , getAllProducts FetchedProducts
    )



-- UPDATE


type Msg
    = ChangeName String
    | ChangePrice String
    | CreateProduct
    | NoOp (Result Http.Error ())
    | FetchProducts
    | FetchedProducts (Result Http.Error (List Api.Product))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp _ ->
            ( model, Cmd.none )

        ChangeName name ->
            ( { model | name = name }, Cmd.none )

        ChangePrice price ->
            ( { model | price = price }, Cmd.none )

        CreateProduct ->
            if model.name /= "" && model.price /= "" then
                case String.toFloat model.price of
                    Just price ->
                        ( model
                        , createProduct
                            { name = model.name
                            , price = price
                            }
                            NoOp
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        -- API
        FetchProducts ->
            ( model, getAllProducts FetchedProducts )

        FetchedProducts products ->
            ( { model
                | products =
                    products
                        |> fromResult
                        |> mapFetched NEList.fromList
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ label []
            [ text "Name:"
            , input
                [ onInput ChangeName
                ]
                []
            ]
        , label []
            [ text "Price:"
            , input
                [ type_ "number"
                , onInput ChangePrice
                ]
                []
            ]
        , button [ onClick CreateProduct ] [ text "Add" ]
        , text model.name
        , text model.price
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
