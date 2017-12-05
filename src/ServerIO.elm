module ServerIO exposing (..)

import Dict exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, bool, dict, index, int, list, map, map2, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode as Encode
import Regex exposing (..)
import Types exposing (..)


decodePercivalData : Decoder PercivalData
decodePercivalData =
    decode PercivalData
        |> required "vol_title" string
        |> required "opts" decodeOpts
        |> required "docs" decodeDocZipper


decodeOpts : Decoder Opts
decodeOpts =
    decode Opts
        |> required "vers" string
        |> required "lang" string


decodeDocZipper : Decoder DocZipper
decodeDocZipper =
    decode DocZipper
        |> hardcoded []
        |> required "first" decodeDocData
        |> required "rest" (list string)


decodeDocData : Decoder DocData
decodeDocData =
    decode DocData
        |> required "docId" string
        |> required "name" string
        |> required "blocks" (list decodeBlockTuple)
        |> required "refs" decodeRefDict


decodeBlockTuple : Decoder BlockTuple
decodeBlockTuple =
    map2 (,) (index 0 string) (index 1 string)


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
        |> optional "possible" (list string) []
        |> optional "confirmed" bool False


decodeMessages : Decoder Messages
decodeMessages =
    decode Messages
        |> required "messages" (list decodeValidatorMessage)


decodeValidatorMessage : Decoder ValidatorMessage
decodeValidatorMessage =
    decode ValidatorMessage
        |> required "message" string


encodeRefData : RefData -> Encode.Value
encodeRefData data =
    Encode.object
        [ ( "scripture", Encode.string data.scripture )
        , ( "valid", Encode.bool data.valid )
        , ( "message", Encode.string data.message )
        , ( "confidence", Encode.int data.confidence )
        , ( "possible"
          , Encode.list
                (data.possible
                    |> List.map (\osis -> Encode.string osis)
                )
          )
        , ( "confirmed", Encode.bool data.confirmed )
        ]


encodeRef : Ref -> Encode.Value
encodeRef ref =
    Encode.object
        [ ( "text", Encode.string ref.text )
        , ( "data", encodeRefData ref.data )
        ]


encodeRefsObj : RefDict -> Encode.Value
encodeRefsObj refs =
    Encode.object
        (Dict.map (\refid ref -> encodeRef ref) refs
            |> Dict.toList
        )


postBlock : String -> Block -> Cmd Msg
postBlock blockId block =
    let
        blockObj =
            Encode.object
                [ ( "html", Encode.string block.html )
                , ( "refs", encodeRefsObj block.refs )
                ]

        postUrl =
            "http://localhost:7777/data/blocks/" ++ blockId
    in
    Http.post postUrl (Http.jsonBody blockObj) decodeBlock
        |> Http.send HandlePostResponse


postFieldInput : String -> Opts -> Cmd Msg
postFieldInput input { versification, language } =
    let
        inputObj =
            Encode.object
                [ ( "input", Encode.string input ) ]

        postUrl =
            "http://localhost:7777/parse?vers="
                ++ versification
                ++ "&lang="
                ++ language
    in
    Http.post postUrl (Http.jsonBody inputObj) decodeRefData
        |> Http.send HandleParserResponse


fetchScripText : Osis -> Cmd Msg
fetchScripText osis =
    Http.getString ("https://ygjutai0z5.execute-api.us-east-1.amazonaws.com/percival/" ++ osis)
        |> Http.send SetScripText


postValidateHtml : String -> Cmd Msg
postValidateHtml html =
    let
        htmlStr =
            "<!DOCTYPE html>\n<html lang=\"en\"><head><title>Validate me</title></head>\n<body>"
                ++ Regex.replace All (regex " epub:type=\"[^\"]+\"") (\_ -> "") html
                ++ "</body></html>"

        postUrl =
            "https://validator.nu/?level=error&out=json"
    in
    Http.post postUrl (Http.stringBody "text/html; charset=utf-8" htmlStr) decodeMessages
        |> Http.send HandleMessages


postNewHtml : String -> String -> Cmd Msg
postNewHtml blockId html =
    let
        htmlObj =
            Encode.object
                [ ( "html", Encode.string html ) ]

        postUrl =
            "http://localhost:7777/data/blocks/" ++ blockId ++ "?in=html"
    in
    Http.post postUrl (Http.jsonBody htmlObj) decodeBlock
        |> Http.send HandlePostHtml


postContext : String -> String -> Opts -> Cmd Msg
postContext blockId context { versification, language } =
    let
        ctxtObj =
            Encode.object
                [ ( "context", Encode.string context ) ]

        postUrl =
            "http://localhost:7777/data/blocks/"
                ++ blockId
                ++ "?with=context&vers="
                ++ versification
                ++ "&lang="
                ++ language
    in
    Http.post postUrl (Http.jsonBody ctxtObj) decodeBlock
        |> Http.send HandlePostHtml
