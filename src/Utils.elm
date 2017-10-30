module Utils exposing (..)

import Array exposing (..)
import Dict exposing (..)
import Types exposing (..)


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


getNearbyIdOfDict : NavDir -> String -> Dict String a -> String
getNearbyIdOfDict navDir currentId dict =
    let
        keysArray =
            Array.fromList (Dict.keys dict)

        currentIndex =
            keysArray
                |> Array.toIndexedList
                |> List.filter (\tup -> Tuple.second tup == currentId)
                |> List.head
                |> (\tup ->
                        case tup of
                            Nothing ->
                                0

                            Just tup ->
                                Tuple.first tup
                   )

        targetIndex =
            case navDir of
                Prev ->
                    currentIndex - 1

                Next ->
                    currentIndex + 1
    in
    keysArray
        |> Array.get targetIndex
        |> (\nextId ->
                case nextId of
                    Nothing ->
                        currentId

                    Just id ->
                        id
           )


getNextDocId : Model -> String
getNextDocId model =
    getNearbyIdOfDict Next model.currentDocId model.percivalData.docs


getPrevDocId : Model -> String
getPrevDocId model =
    getNearbyIdOfDict Prev model.currentDocId model.percivalData.docs


isFullConf : Ref -> Bool
isFullConf ref =
    ref.data.valid && ref.data.confidence == 10


isLowConf : Ref -> Bool
isLowConf ref =
    ref.data.valid && ref.data.confidence < 10


isInvalid : Ref -> Bool
isInvalid ref =
    not ref.data.valid


getRefCount : RefType -> List Ref -> Int
getRefCount refType refList =
    let
        filtered =
            case refType of
                FullConf ->
                    List.filter (\ref -> isFullConf ref) refList

                LowConf ->
                    List.filter (\ref -> isLowConf ref) refList

                Invalid ->
                    List.filter (\ref -> isInvalid ref) refList
    in
    List.length filtered
