module Cart exposing (..)


type alias Item =
    { id : Int
    , name : String
    , price : Float
    }


type alias Cart =
    List Item
