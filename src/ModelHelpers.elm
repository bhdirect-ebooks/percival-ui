module ModelHelpers exposing (..)

import Array exposing (Array, fromList)
import Types exposing (..)


{-
   change : Changes -> ( BlockId, Block ) -> Changes
   change undoList (blockId, block) =
       undoList.

   undo : Changes -> ( BlockId, Block ) -> Changes
   undo undoList (blockId, block) =
-}
-- EditorModel


setHtmlSource : String -> EditorModel -> EditorModel
setHtmlSource html editor =
    { editor | htmlSource = html }


setEditingBlockId : BlockId -> EditorModel -> EditorModel
setEditingBlockId blockId editor =
    { editor | editingBlockId = blockId }


setEditorActive : Bool -> EditorModel -> EditorModel
setEditorActive isActive editor =
    { editor | editorActive = isActive }


setEditorTheme : EditorTheme -> EditorModel -> EditorModel
setEditorTheme theme editor =
    { editor | editorTheme = theme }


setIsValidating : Bool -> EditorModel -> EditorModel
setIsValidating validating editor =
    { editor | isValidating = validating }


setHtmlValidation : List String -> EditorModel -> EditorModel
setHtmlValidation messages editor =
    { editor | htmlValidation = messages }



-- ScriptureListModel


setSelectedRefType : Maybe RefType -> ScriptureListModel -> ScriptureListModel
setSelectedRefType refType scriptureList =
    { scriptureList | selectedRefType = refType }


setDocRefIds : List RefConfTuple -> ScriptureListModel -> ScriptureListModel
setDocRefIds refList scriptureList =
    { scriptureList | docRefIds = refList }


setListedRefs : RefZipper -> ScriptureListModel -> ScriptureListModel
setListedRefs refs scriptureList =
    { scriptureList | listedRefs = refs }


asListedRefsIn : ScriptureListModel -> RefZipper -> ScriptureListModel
asListedRefsIn =
    flip setListedRefs


zipToRef : NavDir -> Maybe Confirmation -> RefZipper -> RefZipper
zipToRef navDir mayConf refs =
    case navDir of
        Prev ->
            if not (List.isEmpty refs.prev) then
                case mayConf of
                    Nothing ->
                        toPrevRef refs

                    Just conf ->
                        let
                            targetId =
                                nextUnconfId (List.reverse refs.prev)
                        in
                        case targetId of
                            Nothing ->
                                refs

                            Just targetId ->
                                toRefId targetId refs
            else
                refs

        Next ->
            if not (List.isEmpty refs.next) then
                case mayConf of
                    Nothing ->
                        toNextRef refs

                    Just conf ->
                        let
                            targetId =
                                nextUnconfId refs.next
                        in
                        case targetId of
                            Nothing ->
                                refs

                            Just targetId ->
                                toRefId targetId refs
            else
                refs

        Id refId ->
            toRefId refId refs


toPrevRef : RefZipper -> RefZipper
toPrevRef refs =
    let
        newCurrent =
            refs.prev
                |> List.reverse
                |> List.head
                |> (\ref ->
                        case ref of
                            Nothing ->
                                ( "", Unconfirmed )

                            Just ref ->
                                ref
                   )

        newPrev =
            refs.prev
                |> List.reverse
                |> List.drop 1
                |> List.reverse

        newNext =
            List.singleton refs.current ++ refs.next
    in
    { prev = newPrev
    , current = newCurrent
    , next = newNext
    }


toNextRef : RefZipper -> RefZipper
toNextRef refs =
    let
        newCurrent =
            refs.next
                |> List.head
                |> (\ref ->
                        case ref of
                            Nothing ->
                                ( "", Unconfirmed )

                            Just ref ->
                                ref
                   )

        newNext =
            refs.next
                |> List.drop 1

        newPrev =
            refs.prev ++ List.singleton refs.current
    in
    { prev = newPrev
    , current = newCurrent
    , next = newNext
    }


nextUnconfId : List RefConfTuple -> Maybe RefId
nextUnconfId refList =
    let
        first =
            List.head refList
    in
    case first of
        Nothing ->
            Nothing

        Just first ->
            case first of
                ( _, Unconfirmed ) ->
                    Just (Tuple.first first)

                _ ->
                    nextUnconfId refList


toRefId : RefId -> RefZipper -> RefZipper
toRefId refId refs =
    let
        inPrev =
            isInRefConfTuple refId refs.prev

        inNext =
            isInRefConfTuple refId refs.next
    in
    if inPrev then
        zipBack refId refs
    else if inNext then
        zipForward refId refs
    else
        refs


zipBack : RefId -> RefZipper -> RefZipper
zipBack refId refs =
    let
        refArray =
            Array.fromList refs.prev

        targetIndex =
            getIndexFromRefConfTupArray refId refArray

        newCurrent =
            getTargetRefConfTupFromArray targetIndex refArray

        newNext =
            List.drop (targetIndex + 1) refs.prev ++ refs.next

        newPrev =
            List.take targetIndex refs.prev
    in
    case newCurrent of
        ( "", _ ) ->
            refs

        _ ->
            { prev = newPrev
            , current = newCurrent
            , next = newNext
            }


zipForward : RefId -> RefZipper -> RefZipper
zipForward refId refs =
    let
        refArray =
            Array.fromList refs.next

        targetIndex =
            getIndexFromRefConfTupArray refId refArray

        newCurrent =
            getTargetRefConfTupFromArray targetIndex refArray

        newNext =
            List.drop (targetIndex + 1) refs.next

        newPrev =
            refs.prev ++ List.take targetIndex refs.prev
    in
    case newCurrent of
        ( "", _ ) ->
            refs

        _ ->
            { prev = newPrev
            , current = newCurrent
            , next = newNext
            }


isInRefConfTuple : RefId -> List RefConfTuple -> Bool
isInRefConfTuple refId refTups =
    refTups
        |> List.unzip
        |> Tuple.first
        |> List.member refId


getIndexFromRefConfTupArray : RefId -> Array RefConfTuple -> Int
getIndexFromRefConfTupArray refId refArray =
    refArray
        |> Array.toIndexedList
        |> List.filter (\( i, ( curId, _ ) ) -> curId == refId)
        |> List.head
        |> (\tup ->
                case tup of
                    Nothing ->
                        0

                    Just tup ->
                        Tuple.first tup
           )


getTargetRefConfTupFromArray : Int -> Array RefConfTuple -> RefConfTuple
getTargetRefConfTupFromArray targetIndex refArray =
    Array.get targetIndex refArray
        |> (\ref ->
                case ref of
                    Nothing ->
                        ( "", Unconfirmed )

                    Just ref ->
                        ref
           )



-- ActionPanelModel


setEditingOsis : Bool -> ActionPanelModel -> ActionPanelModel
setEditingOsis isEditing actionPanel =
    { actionPanel | editingOsis = isEditing }


setOsisField : String -> ActionPanelModel -> ActionPanelModel
setOsisField osisOrMessage actionPanel =
    { actionPanel | osisField = osisOrMessage }


setBadInput : Bool -> ActionPanelModel -> ActionPanelModel
setBadInput isBad actionPanel =
    { actionPanel | badInput = isBad }


setEditingContext : Bool -> ActionPanelModel -> ActionPanelModel
setEditingContext isEditing actionPanel =
    { actionPanel | editingContext = isEditing }


setContextField : String -> ActionPanelModel -> ActionPanelModel
setContextField context actionPanel =
    { actionPanel | contextField = context }


setViewAltRefs : Bool -> ActionPanelModel -> ActionPanelModel
setViewAltRefs viewDemAlts actionPanel =
    { actionPanel | viewAltRefs = viewDemAlts }


setViewScriptureText : Bool -> ActionPanelModel -> ActionPanelModel
setViewScriptureText viewDatScripchaMan actionPanel =
    { actionPanel | viewScriptureText = viewDatScripchaMan }


setScriptureText : String -> ActionPanelModel -> ActionPanelModel
setScriptureText bibleText actionPanel =
    { actionPanel | scriptureText = bibleText }
