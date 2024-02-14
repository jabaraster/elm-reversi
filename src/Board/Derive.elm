module Board.Derive exposing (..)

import Array
import Board exposing (..)
import Dict
import Json.Decode
import Json.Encode
import Position.Derive exposing (decodePosition, encodePosition)
import Set


encodeBoard : Board -> Json.Encode.Value
encodeBoard =
    Json.Encode.list (Json.Encode.list encodeCell)


encodeStone : Stone -> Json.Encode.Value
encodeStone val =
    case val of
        White ->
            Json.Encode.object [ ( "$", Json.Encode.string "White" ) ]

        Black ->
            Json.Encode.object [ ( "$", Json.Encode.string "Black" ) ]


encodeCell : Cell -> Json.Encode.Value
encodeCell =
    \value0 ->
        Json.Encode.object
            [ ( "stone", encodeMaybe encodeStone value0.stone ), ( "position", encodePosition value0.position ) ]


decodeBoard : Json.Decode.Decoder Board
decodeBoard =
    Json.Decode.list (Json.Decode.list decodeCell)


decodeStone : Json.Decode.Decoder Stone
decodeStone =
    Json.Decode.andThen
        (\tag ->
            case tag of
                "White" ->
                    Json.Decode.succeed White

                "Black" ->
                    Json.Decode.succeed Black

                _ ->
                    Json.Decode.fail ("Unexpected tag name: " ++ tag)
        )
        (Json.Decode.field "$" Json.Decode.string)


decodeCell : Json.Decode.Decoder Cell
decodeCell =
    Json.Decode.succeed Cell
        |> decodeAndMap (Json.Decode.field "stone" (Json.Decode.maybe decodeStone))
        |> decodeAndMap (Json.Decode.field "position" decodePosition)


encodeMaybe : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeMaybe f encodeMaybeValue =
    case encodeMaybeValue of
        Nothing ->
            Json.Encode.null

        Just justValue ->
            f justValue


encodeChar : Char -> Json.Encode.Value
encodeChar value =
    Json.Encode.string (String.fromChar value)


encodeResult : (err -> Json.Encode.Value) -> (ok -> Json.Encode.Value) -> Result err ok -> Json.Encode.Value
encodeResult errEncoder okEncoder value =
    case value of
        Err err ->
            Json.Encode.object [ ( "$", Json.Encode.string "Err" ), ( "a", errEncoder err ) ]

        Ok ok ->
            Json.Encode.object [ ( "$", Json.Encode.string "Ok" ), ( "a", okEncoder ok ) ]


decodeChar : Json.Decode.Decoder Char
decodeChar =
    Json.Decode.andThen
        (\str ->
            case String.toList str of
                [ c ] ->
                    Json.Decode.succeed c

                _ ->
                    Json.Decode.fail "decodeChar: too many charactors for Char type"
        )
        Json.Decode.string


decodeResult : Json.Decode.Decoder err -> Json.Decode.Decoder ok -> Json.Decode.Decoder (Result err ok)
decodeResult errDecoder okDecoder =
    Json.Decode.andThen
        (\tag ->
            case tag of
                "Err" ->
                    Json.Decode.map Err (Json.Decode.field "a" errDecoder)

                "Ok" ->
                    Json.Decode.map Ok (Json.Decode.field "a" okDecoder)

                _ ->
                    Json.Decode.fail ("decodeResult: Invalid tag name: " ++ tag)
        )
        (Json.Decode.field "$" Json.Decode.string)


decodeAndMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
decodeAndMap =
    Json.Decode.map2 (|>)
