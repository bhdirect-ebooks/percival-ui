module Decoders exposing (..)

import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, hardcoded, optional, required)
import Types exposing (..)


decodePercivalData : Decoder PercivalData
decodePercivalData =
    decode PercivalData
        |> required "vol_title" string
        |> required "opts" decodeOpts
        |> required "docs" (list decodeDoc)
        |> required "blocks" (list decodeBlock)


decodeOpts : Decoder Opts
decodeOpts =
    decode Opts
        |> required "vers" string
        |> required "lang" string


decodeDoc : Decoder Doc
decodeDoc =
    decode Doc
        |> required "_id" string
        |> required "name" string


decodeBlock : Decoder Block
decodeBlock =
    decode Block
        |> required "_id" string
        |> required "html" string
        |> required "refs" (list decodeRef)
        |> hardcoded False


decodeRef : Decoder Ref
decodeRef =
    decode Ref
        |> required "_id" string
        |> required "text" string
        |> required "data" decodeRefData
        |> hardcoded False


decodeRefData : Decoder RefData
decodeRefData =
    decode RefData
        |> required "scripture" string
        |> optional "valid" bool False
        |> optional "message" string ""
        |> optional "confidence" int 0
        |> optional "possibile" (list string) []
        |> optional "confirmed" bool False
