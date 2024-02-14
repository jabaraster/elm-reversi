module Position.Derive exposing (..)

import Json.Decode
import Json.Encode
import Position exposing (Position)


encodePosition : Position -> Json.Encode.Value
encodePosition =
    \value0 -> Json.Encode.object [ ( "x", Json.Encode.int value0.x ), ( "y", Json.Encode.int value0.y ) ]


decodePosition : Json.Decode.Decoder Position
decodePosition =
    Json.Decode.succeed Position
        |> decodeAndMap (Json.Decode.field "x" Json.Decode.int)
        |> decodeAndMap (Json.Decode.field "y" Json.Decode.int)


decodeAndMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
decodeAndMap =
    Json.Decode.map2 (|>)
