module Utils exposing (..)

import Dict exposing (..)
import Json.Encode as Encode
import Regex exposing (..)
import ServerIO exposing (encodeRefData)
import Types exposing (..)


getOsisWithRefId : RefId -> RefDict -> Osis
getOsisWithRefId refId refs =
    Dict.get refId refs
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


getRefListId : RefId -> String
getRefListId refId =
    "reflist-" ++ refId


getBlockIdFromRefId : RefId -> String
getBlockIdFromRefId refId =
    Regex.replace (AtMost 1) (regex "-\\d\\d\\d$") (\_ -> "") refId


belongsToDoc : DocId -> BlockId -> Bool
belongsToDoc currentDocId blockKey =
    let
        docBlockRegex =
            String.concat [ "^", currentDocId ]
    in
    contains (regex docBlockRegex) blockKey


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



{-
   getDocRefs : RefDict -> List ( RefId, Ref )
   getDocRefs model =
       let
           blockList =
               getDocBlocks model
                   |> Dict.values
       in
       List.map (\block -> block.refs |> Dict.toList) blockList
           |> List.concat
-}


toSimpleRefTup : ( RefId, Ref ) -> ( RefId, Confirmation )
toSimpleRefTup ( refId, ref ) =
    let
        confType =
            assignConfType ref
    in
    ( refId, confType )


getDocRefList : DocData -> List RefConfTuple
getDocRefList docData =
    docData.refs
        |> Dict.toList
        |> List.map toSimpleRefTup


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


getListedRefList : DocData -> Maybe RefType -> List RefConfTuple
getListedRefList docData refType =
    docData.refs
        |> Dict.toList
        |> filterRefs refType
        |> List.map toSimpleRefTup


isInRefTupList : RefId -> List RefConfTuple -> Bool
isInRefTupList refId refTupList =
    refTupList
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
