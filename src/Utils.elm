port module Utils exposing (..)

import Array exposing (..)
import Decoders exposing (decodeBlock, decodeMessages, decodeRefData)
import Dict exposing (..)
import Http exposing (..)
import Json.Encode as Encode
import Regex exposing (..)
import Types exposing (..)


port clickedRef : (String -> msg) -> Sub msg


port scrollList : String -> Cmd msg


port scrollDoc : String -> Cmd msg


port backtoTop : Bool -> Cmd msg


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


getOsisWithRefId : RefId -> BlockDict -> Osis
getOsisWithRefId refId blocks =
    let
        blockId =
            getBlockIdFromRefId refId
    in
    Dict.get blockId blocks
        |> (\mayBlock ->
                case mayBlock of
                    Nothing ->
                        ""

                    Just block ->
                        Dict.get refId block.refs
                            |> (\mayRef ->
                                    case mayRef of
                                        Nothing ->
                                            ""

                                        Just ref ->
                                            if ref.data.valid then
                                                ref.data.scripture
                                            else
                                                ref.data.message
                               )
           )


getRefListId : RefId -> String
getRefListId refId =
    "reflist-" ++ refId


getBlockIdFromRefId : RefId -> String
getBlockIdFromRefId refId =
    Regex.replace (AtMost 1) (regex "-\\d\\d\\d$") (\_ -> "") refId


belongsToDoc : String -> String -> Bool
belongsToDoc currentDocId blockKey =
    let
        docBlockRegex =
            String.concat [ "^", currentDocId ]
    in
    contains (regex docBlockRegex) blockKey


getDocBlocks : Model -> BlockDict
getDocBlocks model =
    model.blockState.present.blocks
        |> Dict.filter (\k v -> belongsToDoc model.currentDocId k)


getFirstIdOfDict : Dict String a -> String
getFirstIdOfDict dict =
    Dict.keys dict
        |> List.head
        |> (\id ->
                case id of
                    Nothing ->
                        ""

                    Just id ->
                        id
           )


findIndexByValue : a -> Array a -> Int
findIndexByValue targetValue array =
    array
        |> Array.toIndexedList
        |> List.filter (\tup -> Tuple.second tup == targetValue)
        |> List.head
        |> (\tup ->
                case tup of
                    Nothing ->
                        0

                    Just tup ->
                        Tuple.first tup
           )


getNearbyIdOfArray : NavDir -> String -> Array String -> String
getNearbyIdOfArray navDir currentId keysArray =
    let
        targetIndex =
            if currentId == "" then
                0
            else
                let
                    currentIndex =
                        findIndexByValue currentId keysArray
                in
                case navDir of
                    Prev ->
                        currentIndex - 1

                    Next ->
                        currentIndex + 1
    in
    keysArray
        |> Array.get targetIndex
        |> (\targetId ->
                case targetId of
                    Nothing ->
                        currentId

                    Just targetId ->
                        targetId
           )


getNearbyIdOfDict : NavDir -> String -> Dict String a -> String
getNearbyIdOfDict navDir currentId dict =
    let
        keysArray =
            Array.fromList (Dict.keys dict)
    in
    getNearbyIdOfArray navDir currentId keysArray


getNearbyDocId : NavDir -> Model -> String
getNearbyDocId navDir model =
    getNearbyIdOfDict navDir model.currentDocId model.percivalData.docs


getNearbyRefId : NavDir -> RefId -> RefIdArray -> RefId
getNearbyRefId navDir currentRefId refTupArray =
    let
        refIdArr =
            Array.map (\refIdTup -> Tuple.first refIdTup) refTupArray
    in
    getNearbyIdOfArray navDir currentRefId refIdArr


getNearbyUnconfId : NavDir -> RefId -> RefIdArray -> RefId
getNearbyUnconfId navDir currentRefId refTupArray =
    let
        filteredRefIdArr =
            refTupArray
                |> Array.filter (\( refId, conf ) -> conf == Unconfirmed || refId == currentRefId)
                |> Array.map (\refIdTup -> Tuple.first refIdTup)
    in
    getNearbyIdOfArray navDir currentRefId filteredRefIdArr


isConfirmed : Ref -> Bool
isConfirmed ref =
    ref.data.confirmed && not (isInvalid ref)


isLowConf : Ref -> Bool
isLowConf ref =
    not (isConfirmed ref) && ref.data.valid && ref.data.confidence < 10


isFullConf : Ref -> Bool
isFullConf ref =
    ref.data.confidence == 10


isInvalid : Ref -> Bool
isInvalid ref =
    not ref.data.valid


isUnconfirmed : Ref -> Bool
isUnconfirmed ref =
    not (isConfirmed ref)


assignConfType : Ref -> Confirmation
assignConfType ref =
    case isConfirmed ref of
        True ->
            Confirmed

        False ->
            Unconfirmed


getRefCount : RefType -> List Ref -> Int
getRefCount refType refList =
    let
        filtered =
            case refType of
                Confirm Confirmed ->
                    List.filter (\ref -> isConfirmed ref) refList

                Confirm Unconfirmed ->
                    List.filter (\ref -> isUnconfirmed ref) refList

                RefConf NotFull ->
                    List.filter (\ref -> isLowConf ref) refList

                RefConf Full ->
                    List.filter (\ref -> isFullConf ref) refList

                RefVal Invalid ->
                    List.filter (\ref -> isInvalid ref) refList

                RefVal Valid ->
                    List.filter (\ref -> not (isInvalid ref)) refList
    in
    List.length filtered


getDocRefs : Model -> List ( RefId, Ref )
getDocRefs model =
    let
        blockList =
            getDocBlocks model
                |> Dict.values
    in
    List.map (\block -> block.refs |> Dict.toList) blockList
        |> List.concat


toSimpleRefTup : ( RefId, Ref ) -> ( RefId, Confirmation )
toSimpleRefTup ( refId, ref ) =
    let
        confType =
            assignConfType ref
    in
    ( refId, confType )


getDocRefArray : Model -> RefIdArray
getDocRefArray model =
    getDocRefs model
        |> List.map toSimpleRefTup
        |> Array.fromList


filterRefs : Maybe RefType -> List ( RefId, Ref ) -> List ( RefId, Ref )
filterRefs mayRefType refTupList =
    case mayRefType of
        Nothing ->
            refTupList

        Just refType ->
            case refType of
                Confirm Confirmed ->
                    List.filter (\( k, v ) -> isConfirmed v) refTupList

                Confirm Unconfirmed ->
                    List.filter (\( k, v ) -> isUnconfirmed v) refTupList

                RefConf NotFull ->
                    List.filter (\( k, v ) -> isLowConf v) refTupList

                RefConf Full ->
                    List.filter (\( k, v ) -> isFullConf v) refTupList

                RefVal Invalid ->
                    List.filter (\( k, v ) -> isInvalid v) refTupList

                RefVal Valid ->
                    List.filter (\( k, v ) -> not (isInvalid v)) refTupList


getListedRefArray : Model -> RefIdArray
getListedRefArray model =
    getDocRefs model
        |> filterRefs model.selectedRefType
        |> List.map toSimpleRefTup
        |> Array.fromList


isInRefIdArray : RefId -> RefIdArray -> Bool
isInRefIdArray refId refIdArray =
    refIdArray
        |> Array.toList
        |> List.filter (\tup -> Tuple.first tup == refId)
        |> List.isEmpty
        |> not


getUpdatedBlock : Regex -> RefStuff -> Block -> Block
getUpdatedBlock origTagRegex { refId, ref, refDP } block =
    case refDP of
        Remove ->
            let
                newRefDict =
                    block.refs
                        |> Dict.remove refId

                newHtml =
                    block.html
                        |> replace (AtMost 1) origTagRegex (\_ -> ref.text)
            in
            { block | html = newHtml, refs = newRefDict }

        _ ->
            let
                newRef =
                    updateRefData ref refDP

                newRefDict =
                    block.refs
                        |> Dict.update refId (always (Just newRef))

                newTag =
                    "<a data-cross-ref='"
                        ++ Encode.encode 0 (encodeRefData newRef.data)
                        ++ "'>"
                        ++ ref.text
                        ++ "</a>"

                newHtml =
                    block.html
                        |> replace (AtMost 1) origTagRegex (\_ -> newTag)
            in
            { block | html = newHtml, refs = newRefDict }


updateBlockRef : RefId -> RefDataPoint -> Block -> Block
updateBlockRef refId refDP block =
    let
        ref =
            Dict.get refId block.refs
    in
    case ref of
        Nothing ->
            block

        Just ref ->
            let
                origTagRegex =
                    regex
                        ("<a data-cross-ref=(?:\"|'){(?:&quot;|\\\")scripture(?:&quot;|\\\"):(?:&quot;|\\\")"
                            ++ ref.data.scripture
                            ++ "[^}]+}(\"|')>"
                            ++ escape ref.text
                            ++ "</a>"
                        )

                refStuff =
                    { refId = refId
                    , ref = ref
                    , refDP = refDP
                    }
            in
            getUpdatedBlock origTagRegex refStuff block


getRevisedAltList : Osis -> RefData -> List Osis
getRevisedAltList newOsis data =
    let
        currentOsis =
            data.scripture
    in
    if not (currentOsis == "") then
        if List.member newOsis data.possible && not (newOsis == "") then
            if List.member currentOsis data.possible then
                data.possible
                    |> List.filter (\altOsis -> not (altOsis == newOsis))
            else
                data.possible
                    |> List.map
                        (\altOsis ->
                            if altOsis == newOsis then
                                currentOsis
                            else
                                altOsis
                        )
        else
            data.possible
                |> List.append [ currentOsis ]
    else
        data.possible


updateRefData : Ref -> RefDataPoint -> Ref
updateRefData ref refDP =
    let
        data =
            ref.data

        newData =
            case refDP of
                Scripture osis ->
                    let
                        altList =
                            getRevisedAltList osis data
                    in
                    { data
                        | scripture = osis
                        , valid = True
                        , message = ""
                        , confirmed = True
                        , possible = altList
                    }

                UserConf userConf ->
                    let
                        confirmation =
                            case userConf of
                                Confirmed ->
                                    True

                                Unconfirmed ->
                                    False
                    in
                    { data | confirmed = confirmation }

                UserVal valid message ->
                    let
                        altList =
                            getRevisedAltList "" data
                    in
                    if valid == Invalid then
                        { data
                            | scripture = ""
                            , valid = False
                            , message = message
                            , confirmed = False
                            , possible = altList
                        }
                    else
                        data

                _ ->
                    data
    in
    { ref | data = newData }


toXhtml : String -> String
toXhtml html =
    html
        |> Regex.replace All
            (caseInsensitive (regex "([a-z:]+=)'([^']+)'"))
            (\{ submatches } ->
                case submatches of
                    first :: second :: _ ->
                        "\""
                            |> String.append (Maybe.withDefault "" second)
                            |> String.append "\""
                            |> String.append (Maybe.withDefault "" first)

                    _ ->
                        ""
            )
        |> Regex.replace All
            (caseInsensitive (regex "<(link|hr|img|source)([^>]+)>"))
            (\{ submatches } ->
                case submatches of
                    first :: second :: _ ->
                        " />"
                            |> String.append (Maybe.withDefault "" second)
                            |> String.append (Maybe.withDefault "" first)
                            |> String.append "<"

                    _ ->
                        ""
            )
        |> Regex.replace All (caseInsensitive (regex "<br>")) (\_ -> "<br />")
        |> Regex.replace All
            (caseInsensitive (regex "<video(.*?)controls(.*?)>"))
            (\{ submatches } ->
                case submatches of
                    first :: second :: _ ->
                        ">"
                            |> String.append (Maybe.withDefault "" second)
                            |> String.append "controls=\"controls\""
                            |> String.append (Maybe.withDefault "" first)
                            |> String.append "<video"

                    _ ->
                        ""
            )
        |> Regex.replace All (caseInsensitive (regex "alt=\"0\"")) (\_ -> "alt=\"\"")
        |> Regex.replace All
            (caseInsensitive (regex "(>[^<>\n]*?)&(?!#)([^<>\n]*?<)"))
            (\{ submatches } ->
                case submatches of
                    first :: second :: _ ->
                        Maybe.withDefault "" second
                            |> String.append "&#38;"
                            |> String.append (Maybe.withDefault "" first)

                    _ ->
                        ""
            )
        |> Regex.replace All
            (caseInsensitive (regex "\"({[^}]+})\""))
            (\{ submatches } ->
                case submatches of
                    first :: _ ->
                        "'"
                            |> String.append (Maybe.withDefault "" first)
                            |> String.append "'"

                    _ ->
                        ""
            )
        |> Regex.replace All (caseInsensitive (regex "&quot;")) (\_ -> "\"")


normalizeBlockHtml : Block -> Block
normalizeBlockHtml block =
    let
        normHtml =
            toXhtml block.html
    in
    { block | html = normHtml }


ingestBlocks : BlockDict -> BlockDict
ingestBlocks blocks =
    Dict.map (\k v -> normalizeBlockHtml v) blocks
