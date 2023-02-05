module Main exposing (..)

import Api exposing (baseUrl, createProduct, getAllProducts)
import Browser
import Cart exposing (Cart)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, img, input, label, li, p, text, textarea, ul)
import Html.Attributes exposing (src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Icons.Add exposing (add)
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
    , productImage : Maybe File
    , products : ApiCall Http.Error (Maybe (Nonempty Api.Product))
    , cart : Cart
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { name = ""
      , price = ""
      , products = Fetching
      , cart = []
      , productImage = Nothing
      }
    , getAllProducts FetchedProducts
    )



-- UPDATE


type Msg
    = ChangeName String
    | ChangePrice String
    | CreateProduct
    | FetchProducts
    | FetchedProducts (Result Http.Error (List Api.Product))
    | AddToCart Int
    | ProductImageUploadRequested
    | ProductImageSelected File


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                            , productImage = model.productImage
                            }
                            (\l -> FetchProducts)
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        AddToCart id ->
            case model.products of
                Fetched (Just products) ->
                    case
                        NEList.foldl
                            (\product acc ->
                                if product.id == id then
                                    Just product

                                else
                                    acc
                            )
                            Nothing
                            products
                    of
                        Just product ->
                            ( { model | cart = model.cart |> Cart.addToCart product }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ProductImageUploadRequested ->
            ( model, Select.file [ "img/*" ] ProductImageSelected )

        ProductImageSelected file ->
            ( { model | productImage = Just file }, Cmd.none )

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
        , case model.products of
            Fetching ->
                text "Fetching..."

            Fetched (Just products) ->
                ul
                    [ style "padding" "32px"
                    , style "list-style" "none"
                    , style "display" "grid"
                    , style "gap" "32px"
                    , style "grid-template-columns" "repeat(auto-fit, minmax(133px, 1fr))"
                    ]
                    (products
                        |> NEList.foldl
                            (\product acc ->
                                li [] [ productCard product ] :: acc
                            )
                            []
                    )

            -- TODO: style
            Fetched Nothing ->
                text "No products"

            -- TODO:
            Failed error ->
                text "Error"
        , text "Cart:"
        , ul [] (List.map (\l -> div [] [ text l.name, text ("Amount" ++ String.fromInt l.quantity) ]) model.cart)
        , button [ onClick ProductImageUploadRequested ] [ text "Upload image" ]
        ]


productCard : Api.Product -> Html Msg
productCard product =
    div []
        [ img
            [ src (baseUrl ++ "/images/" ++ String.fromInt product.id)
            , style "width" "100%"
            , style "height" "215px"
            , style "object-fit" "cover"
            ]
            []
        , p [ style "font-size" "16px", style "font-weight" "300" ] [ text product.name ]
        , p [ style "font-size" "14px", style "font-weight" "bold" ] [ text ("$" ++ String.fromFloat product.price) ]
        , button [ onClick (AddToCart product.id) ] [ text "Add to cart" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
