module Decoders exposing (..)

import Json.Decode as Decode exposing (Decoder, bool, dict, int, list, map, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, hardcoded, optional, required)
import Types exposing (..)


decodePercivalData : Decoder PercivalData
decodePercivalData =
    decode PercivalData
        |> required "vol_title" string
        |> required "opts" decodeOpts
        |> required "docs" decodeDocDict
        |> required "blocks" decodeBlockDict


decodeOpts : Decoder Opts
decodeOpts =
    decode Opts
        |> required "vers" string
        |> required "lang" string


decodeDocDict : Decoder DocDict
decodeDocDict =
    dict decodeDoc


decodeDoc : Decoder Doc
decodeDoc =
    decode Doc
        |> required "name" string


decodeBlockDict : Decoder BlockDict
decodeBlockDict =
    dict decodeBlock


decodeBlock : Decoder Block
decodeBlock =
    decode Block
        |> required "html" string
        |> required "refs" decodeRefDict


decodeRefDict : Decoder RefDict
decodeRefDict =
    dict decodeRef


decodeRef : Decoder Ref
decodeRef =
    decode Ref
        |> required "text" string
        |> required "data" decodeRefData


decodeRefData : Decoder RefData
decodeRefData =
    decode RefData
        |> required "scripture" string
        |> optional "valid" bool False
        |> optional "message" string ""
        |> optional "confidence" int 0
        |> optional "possibile" (list string) []
        |> optional "confirmed" bool False
