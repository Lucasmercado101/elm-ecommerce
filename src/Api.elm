module Api exposing (..)

import File exposing (File)
import Http
import Json.Decode as JD exposing (Decoder, field)
import Json.Encode as JE



-- Temp


baseUrl : String
baseUrl =
    "http://localhost:3000"


type alias Product =
    { id : Int
    , name : String
    , price : Float
    }


type alias CreateProductBody =
    { name : String
    , price : Float
    , productImage : Maybe File
    }


ignoreHttpResp : (Result Http.Error () -> msg) -> Cmd msg
ignoreHttpResp msg =
    msg |> (\l -> Cmd.none)


createProduct : CreateProductBody -> (Result Http.Error () -> msg) -> Cmd msg
createProduct body msg =
    Http.post
        { url = baseUrl ++ "/createProduct"
        , body = body |> createProductBodyEncoder |> Http.multipartBody
        , expect = Http.expectWhatever msg
        }


createProductBodyEncoder : CreateProductBody -> List Http.Part
createProductBodyEncoder data =
    [ Http.stringPart "name" data.name
    , Http.stringPart "price" (String.fromFloat data.price)
    ]
        ++ (case data.productImage of
                Just file ->
                    [ Http.filePart "productImage" file ]

                Nothing ->
                    []
           )


getAllProductsDecoder : Decoder (List Product)
getAllProductsDecoder =
    JD.list
        (JD.map3 Product
            (field "id" JD.int)
            (field "name" JD.string)
            (field "price" JD.float)
        )


getAllProducts : (Result Http.Error (List Product) -> msg) -> Cmd msg
getAllProducts msg =
    Http.get
        { url = baseUrl ++ "/getAllProducts"
        , expect = Http.expectJson msg getAllProductsDecoder
        }
