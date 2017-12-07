module ModelHelpers exposing (..)

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


toRef : 