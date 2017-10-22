module Decoders exposing (..)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Types exposing (..)


decodeFileData : Decoder FileData
decodeFileData =
    decode FileData
        |> required "title" string
        |> required "structure" (list string)
        |> required "files" (list decodeFile)
        |> required "toc" decodeToc
        |> required "context" string


decodeFile : Decoder File
decodeFile =
    decode File
        |> required "name" string
        |> required "issues" (list decodeIssue)
        |> required "scripture" (list decodeIssue)
        |> required "outline" string
        |> required "lists" string
        |> required "tables" (list decodeSection)
        |> required "quotes" (list decodeSection)
        |> required "asides" (list decodeSection)


decodeIssue : Decoder Issue
decodeIssue =
    decode Issue
        |> required "line" string
        |> required "col" string
        |> required "message" string
        |> required "help" string
        |> required "class" decodeIssueClass


decodeIssueClass : Decoder IssueClass
decodeIssueClass =
    Decode.map classToIssueClass string


classToIssueClass : String -> IssueClass
classToIssueClass class =
    case class of
        "error" ->
            IssueError

        "warning" ->
            IssueWarning

        "light-warning" ->
            IssueLight

        _ ->
            IssueError


decodeSection : Decoder Section
decodeSection =
    decode Section
        |> required "line" int
        |> required "html" string


decodeToc : Decoder Toc
decodeToc =
    decode Toc
        |> required "user" string
        |> required "comp" string
