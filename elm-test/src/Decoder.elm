module Decoder exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline as P


type alias User =
    { id : Int
    , name : String
    , email : Maybe String
    , isLoggedIn : Bool
    }


userDecoder =
    D.succeed User
        |> P.required "id" D.int
        |> P.required "name" D.string
        |> P.optional "email" (D.nullable D.string) Nothing
        |> P.required "isLoggedIn" D.bool


run =
    let
        jsonString =
            """{ "id": 1, "name": "Bob", "isLoggedIn": true }"""

        res =
            D.decodeString userDecoder jsonString
    in
    case res of
        Ok val ->
            Just val

        Err error ->
            let
                _ =
                    Debug.log "error" error
            in
            Nothing
