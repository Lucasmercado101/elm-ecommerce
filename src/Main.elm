module Main exposing (..)

import Api exposing (baseUrl, createProduct, getAllProducts)
import Browser
import Cart exposing (Cart)
import Color
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, img, input, label, li, p, text, textarea, ul)
import Html.Attributes exposing (src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import List.Extra
import List.Nonempty as NEList exposing (Nonempty(..))
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))


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
    , favorites : List Api.Product
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { name = ""
      , price = ""
      , products = Fetching
      , cart = []
      , productImage = Nothing
      , favorites = []
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
    | ToggleAddToFavorites Int


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

        ToggleAddToFavorites id ->
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
                            case List.Extra.find (\p -> p.id == id) model.favorites of
                                Just _ ->
                                    ( { model
                                        | favorites =
                                            model.favorites
                                                |> List.filter (\p -> p.id /= id)
                                      }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( { model | favorites = product :: model.favorites }
                                    , Cmd.none
                                    )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
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
        , case model.products of
            Fetching ->
                text "Fetching..."

            Fetched (Just products) ->
                ul
                    [ style "padding" "32px"
                    , style "list-style" "none"
                    , style "display" "grid"
                    , style "gap" "32px"
                    , style "grid-template-columns" "repeat(auto-fit, minmax(200px, 1fr))"
                    ]
                    (products
                        |> NEList.foldl
                            (\product acc ->
                                li []
                                    [ productCard
                                        (model.favorites
                                            |> List.Extra.find (\l -> l.id == product.id)
                                            |> maybeToBoolean
                                        )
                                        product
                                    ]
                                    :: acc
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


productCard : Bool -> Api.Product -> Html Msg
productCard favorite product =
    div []
        [ div [ style "position" "relative" ]
            [ img
                [ src (baseUrl ++ "/images/" ++ String.fromInt product.id)
                , style "width" "100%"
                , style "height" "215px"
                , style "object-fit" "cover"
                , style "margin-bottom" "8px"
                ]
                []
            , button
                [ style "position" "absolute"
                , style "background-color" "rgba(0, 0, 0, 0.5)"
                , style "top" "8px"
                , style "right" "8px"
                , style "border-radius" "50%"
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "padding" "8px"
                , style "outline" "none"
                , style "border" "transparent"
                , onClick (ToggleAddToFavorites product.id)
                ]
                [ (if favorite then
                    Filled.favorite

                   else
                    Outlined.favorite_border
                  )
                    18
                    (Color <| Color.rgb255 205 115 115)
                ]
            ]
        , div [ style "display" "flex", style "flex-direction" "row", style "justify-content" "space-between" ]
            [ div [ style "display" "flex", style "flex-direction" "column" ]
                [ p [ style "font-size" "18px", style "font-weight" "300" ] [ text product.name ]
                , p [ style "font-size" "16px", style "font-weight" "bold" ] [ text ("$" ++ String.fromFloat product.price) ]
                ]
            , button
                [ style "background-color" "transparent"
                , style "border" "none"
                , style "outline" "none"
                , onClick (AddToCart product.id)
                ]
                [ Filled.add 32 (Color <| Color.white) ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HELPERS


maybeToBoolean : Maybe a -> Bool
maybeToBoolean maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False
