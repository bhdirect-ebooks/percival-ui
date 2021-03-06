module Utils exposing
    ( assignConfType
    , belongsToDoc
    , findIndexByValue
    , getBlockIdFromRefId
    , getDashboard
    , getDocBlocks
    , getDocRefArray
    , getDocRefCounts
    , getDocRefs
    , getFirstIdOfDict
    , getListedRefArray
    , getListedRefArrayBySelected
    , getListofBlockTups
    , getNearbyDocId
    , getNearbyIdOfArray
    , getNearbyIdOfDict
    , getNearbyRefId
    , getNearbyUnconfId
    , getOsisWithRefId
    , getRefCount
    , getRefListId
    , getRefs
    , getRevisedAltList
    , getTagRegex
    , getTotals
    , getUpdatedBlock
    , isConfirmed
    , isFullConf
    , isInRefIdArray
    , isInvalid
    , isLowConf
    , isUnconfirmed
    , toSimpleRefTup
    , updateBlockRef
    , updateDashboard
    , updateListofBlockRefs
    , updateRefData
    )

import Array exposing (..)
import Dict exposing (..)
import Regex exposing (..)
import Types exposing (..)


getRefs : String -> Model -> List Ref
getRefs docId model =
    getDocRefs { model | currentDocId = docId }
        |> List.map (\( _, refs ) -> refs)


getTotals : List DocStats -> Stats
getTotals docStats =
    let
        totalConf =
            docStats
                |> List.map (\( _, docStats ) -> docStats.stats.confirmed)
                |> List.foldr (+) 0

        totalLow =
            docStats
                |> List.map (\( _, docStats ) -> docStats.stats.lowConfidence)
                |> List.foldr (+) 0

        totalInvalid =
            docStats
                |> List.map (\( _, docStats ) -> docStats.stats.invalid)
                |> List.foldr (+) 0
    in
    { confirmed = totalConf
    , lowConfidence = totalLow
    , invalid = totalInvalid
    }


getDashboard : Model -> DocDict -> Dashboard
getDashboard model trueDocs =
    let
        docStats =
            trueDocs
                |> Dict.toList
                |> List.map
                    (\( docId, doc ) ->
                        ( docId
                        , { name = doc.name
                          , stats =
                                getRefs docId model
                                    |> getDocRefCounts
                          }
                        )
                    )

        totals =
            getTotals docStats
    in
    { totals = totals
    , docStats = docStats
    }


getDocRefCounts : List Ref -> Stats
getDocRefCounts refList =
    let
        confCnt =
            getRefCount (Confirm Confirmed) refList

        lowCnt =
            getRefCount (RefConf NotFull) refList

        invalidCnt =
            getRefCount (RefVal Invalid) refList
    in
    { confirmed = confCnt
    , lowConfidence = lowCnt
    , invalid = invalidCnt
    }


updateDashboard : String -> Model -> Dashboard
updateDashboard blockId model =
    let
        docStats =
            model.dashboard.docStats
                |> List.map
                    (\( id, docStat ) ->
                        if contains (regex ("^" ++ id)) blockId then
                            ( id
                            , { docStat
                                | stats =
                                    getRefs id model
                                        |> getDocRefCounts
                              }
                            )

                        else
                            ( id, docStat )
                    )

        totals =
            getTotals docStats
    in
    { totals = totals
    , docStats = docStats
    }


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
    Regex.replace (AtMost 1) (regex "-\\d\\d\\d\\d$") (\_ -> "") refId


belongsToDoc : String -> String -> Bool
belongsToDoc currentDocId blockId =
    let
        docBlockRegex =
            String.concat [ "^", currentDocId ]
    in
    contains (regex docBlockRegex) blockId


belongsToBlock : String -> RefId -> Bool
belongsToBlock blockId refId =
    let
        blockRegex =
            String.concat [ "^", blockId ]
    in
    contains (regex blockRegex) refId


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
    getNearbyIdOfDict navDir model.currentDocId model.docs


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


filterRefsByType : Maybe RefType -> List ( RefId, Ref ) -> List ( RefId, Ref )
filterRefsByType mayRefType refTupList =
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
        |> filterRefsByType model.selectedRefType
        |> List.map toSimpleRefTup
        |> Array.fromList


filterRefsBySelected : List RefId -> List ( RefId, Ref ) -> List ( RefId, Ref )
filterRefsBySelected refIdList refTupList =
    List.filter (\( k, v ) -> List.member k refIdList) refTupList


getListedRefArrayBySelected : Model -> RefIdArray
getListedRefArrayBySelected model =
    getDocRefs model
        |> filterRefsBySelected model.selectedRefIds
        |> List.map toSimpleRefTup
        |> Array.fromList


isInRefIdArray : RefId -> RefIdArray -> Bool
isInRefIdArray refId refIdArray =
    refIdArray
        |> Array.toList
        |> List.filter (\tup -> Tuple.first tup == refId)
        |> List.isEmpty
        |> not


getTagRegex : RefId -> String -> Regex
getTagRegex refId text =
    regex
        ("<a data-ref[^>]+?"
            ++ refId
            ++ "[^>]+>"
            ++ escape text
            ++ "</a>"
        )


getUpdatedBlock : Regex -> RefStuff -> Block -> Block
getUpdatedBlock tagRegex { refId, ref, refDP } block =
    case refDP of
        Remove ->
            let
                newRefDict =
                    block.refs
                        |> Dict.remove refId

                newHtml =
                    block.html
                        |> replace (AtMost 1) tagRegex (\_ -> ref.text)
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
                    "<a data-ref=\""
                        ++ newRef.data.scripture
                        ++ "\" id=\""
                        ++ refId
                        ++ "\">"
                        ++ ref.text
                        ++ "</a>"

                newHtml =
                    block.html
                        |> replace (AtMost 1) tagRegex (\_ -> newTag)
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
                tagRegex =
                    getTagRegex refId ref.text

                refStuff =
                    { refId = refId
                    , ref = ref
                    , refDP = refDP
                    }
            in
            getUpdatedBlock tagRegex refStuff block


doUpdateList : List RefId -> RefDataPoint -> Block -> Block
doUpdateList refIdList refDP block =
    case refIdList of
        [] ->
            block

        head :: tail ->
            block
                |> updateBlockRef head refDP
                |> doUpdateList tail refDP


updateListofBlockRefs : List RefId -> RefDataPoint -> String -> Block -> Block
updateListofBlockRefs refIdList refDP blockId block =
    let
        blockRefIds =
            List.filter (\refId -> belongsToBlock blockId refId) refIdList
    in
    doUpdateList blockRefIds refDP block


getListofBlockTups : List String -> Model -> List ( String, Block )
getListofBlockTups blockIds model =
    blockIds
        |> List.map
            (\blockId ->
                ( blockId, Dict.get blockId model.blockState.present.blocks )
            )
        |> List.map
            (\( blockId, mayBlock ) ->
                case mayBlock of
                    Nothing ->
                        []

                    Just block ->
                        [ ( blockId, block ) ]
            )
        |> List.concat


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
