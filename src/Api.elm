module Api exposing (..)

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE



-- Temp


baseUrl : String
baseUrl =
    "http://localhost:3000"


type alias Product =
    { id : String
    , name : String
    , price : Float
    }


type alias CreateProductBody =
    { name : String
    , price : Float
    }


ignoreHttpResp : (Result Http.Error () -> msg) -> Cmd msg
ignoreHttpResp msg =
    msg |> (\l -> Cmd.none)


createProduct : CreateProductBody -> (Result Http.Error () -> msg) -> Cmd msg
createProduct body msg =
    Http.post
        { url = baseUrl ++ "/initial"
        , body = body |> createProductBodyEncoder |> Http.jsonBody
        , expect = Http.expectWhatever msg
        }


createProductBodyEncoder : CreateProductBody -> JE.Value
createProductBodyEncoder data =
    JE.object
        [ ( "name", JE.string data.name )
        , ( "price", JE.float data.price )
        ]


createProductsResponseDecoder : Decoder (List Product)
createProductsResponseDecoder =
    JD.list
        (JD.map3 Product
            (JD.field "id" JD.string)
            (JD.field "name" JD.string)
            (JD.field "price" JD.float)
        )
