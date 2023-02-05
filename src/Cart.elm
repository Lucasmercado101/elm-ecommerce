module Cart exposing (..)

import List.Extra


type alias Item =
    { id : Int
    , name : String
    , price : Float
    , quantity : Int
    }


type alias Cart =
    List Item


addToCart :
    { id : Int
    , name : String
    , price : Float
    }
    -> Cart
    -> Cart
addToCart item cart =
    case List.Extra.find (\i -> i.id == item.id) cart of
        Just existingItem ->
            { existingItem | quantity = existingItem.quantity + 1 } :: List.filter (\i -> i.id /= item.id) cart

        Nothing ->
            { id = item.id
            , name = item.name
            , price = item.price
            , quantity = 1
            }
                :: cart
