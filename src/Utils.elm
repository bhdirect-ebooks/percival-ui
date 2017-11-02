port module Utils exposing (..)

import Array exposing (..)
import Dict exposing (..)
import Regex exposing (..)
import Types exposing (..)


port clickedRef : (String -> msg) -> Sub msg


port scrollList : String -> Cmd msg


port scrollDoc : String -> Cmd msg


port backtoTop : Bool -> Cmd msg


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
    model.blockState.present
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


getNearbyRefId : NavDir -> RefId -> Array ( RefId, Confirmation ) -> RefId
getNearbyRefId navDir currentRefId refTupArray =
    let
        refIdArr =
            Array.map (\refIdTup -> Tuple.first refIdTup) refTupArray
    in
    getNearbyIdOfArray navDir currentRefId refIdArr


getNearbyUnconfId : NavDir -> RefId -> Array ( RefId, Confirmation ) -> RefId
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
                UserConf Confirmed ->
                    List.filter (\ref -> isConfirmed ref) refList

                UserConf Unconfirmed ->
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


getDocRefArray : Model -> Array ( RefId, Confirmation )
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
                UserConf Confirmed ->
                    List.filter (\( k, v ) -> isConfirmed v) refTupList

                UserConf Unconfirmed ->
                    List.filter (\( k, v ) -> isUnconfirmed v) refTupList

                RefConf NotFull ->
                    List.filter (\( k, v ) -> isLowConf v) refTupList

                RefConf Full ->
                    List.filter (\( k, v ) -> isFullConf v) refTupList

                RefVal Invalid ->
                    List.filter (\( k, v ) -> isInvalid v) refTupList

                RefVal Valid ->
                    List.filter (\( k, v ) -> not (isInvalid v)) refTupList


getListedRefArray : Model -> Array ( RefId, Confirmation )
getListedRefArray model =
    getDocRefs model
        |> filterRefs model.selectedRefType
        |> List.map toSimpleRefTup
        |> Array.fromList
