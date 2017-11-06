port module Utils exposing (..)

import Array exposing (..)
import Dict exposing (..)
import Http exposing (..)
import Json.Decode exposing (list, string)
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


postBlock : String -> Block -> Http.Request (List String)
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
    Http.post postUrl (Http.jsonBody blockObj) (list string)


fetchScripText : Osis -> Cmd Msg
fetchScripText osis =
    Http.getString ("https://ygjutai0z5.execute-api.us-east-1.amazonaws.com/percival/" ++ osis)
        |> Http.send SetScripText


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
                                            ref.data.scripture
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



{-
   getBlockWithNewRef : Osis -> RefId -> Block -> Block
   getBlockWithNewRef osis refId block =
       let
           ref =
               Dict.get refId block.refs
       in
       case ref of
           Nothing ->
               block

           Just ref ->
               let
                   newAltList =
                       ref.data.possible
                           |> List.filter (\alt -> not (alt == osis))
                           |> List.append [ ref.data.scripture ]

                   newRef =
                       { ref
                           | data =
                               { scripture = osis
                               , valid = True
                               , message = ""
                               , confidence = 10
                               , possible = newAltList
                               , confirmed = True
                               }
                       }

                   newRefDict =
                       Dict.update refId (always (Just newRef)) block.refs

                   newHtml =
                       replaceRefTag osis ref

                   newBlock =
                       { block
                           | html = newHtml
                           , refs = newRefs
                       }
               in
               []
-}
