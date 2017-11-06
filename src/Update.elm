module Update exposing (..)

import Dict exposing (..)
import Keyboard.Combo
import Types exposing (..)
import UndoList exposing (UndoList)
import Utils exposing (..)


keyboardCombos : List (Keyboard.Combo.KeyCombo Msg)
keyboardCombos =
    [ Keyboard.Combo.combo1 Keyboard.Combo.q (ToDoc Prev)
    , Keyboard.Combo.combo1 Keyboard.Combo.w (ToDoc Next)
    , Keyboard.Combo.combo1 Keyboard.Combo.h ToggleHelp
    , Keyboard.Combo.combo1 Keyboard.Combo.up (ToRef Prev Nothing)
    , Keyboard.Combo.combo1 Keyboard.Combo.down (ToRef Next Nothing)
    , Keyboard.Combo.combo1 Keyboard.Combo.left (ToRef Prev (Just Unconfirmed))
    , Keyboard.Combo.combo1 Keyboard.Combo.right (ToRef Next (Just Unconfirmed))
    , Keyboard.Combo.combo1 Keyboard.Combo.escape (ListRefsByType Nothing)
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            model ! []

        NoOp (Err err) ->
            let
                _ =
                    Debug.log "Err" err
            in
            model ! []

        NoOp (Ok _) ->
            model ! []

        LoadData (Ok data) ->
            let
                newModel =
                    { model
                        | percivalData = data
                        , blockState = UndoList.fresh data.blocks
                        , currentDocId = getFirstIdOfDict data.docs
                    }

                docRefArray =
                    getDocRefArray newModel
            in
            { newModel
                | docRefIds = docRefArray
                , listedRefIds = docRefArray
            }
                ! []

        LoadData (Err err) ->
            let
                _ =
                    Debug.log "Err" err
            in
            { model | loadingError = Just "Something went wrong." } ! []

        ComboMsg msg ->
            if not model.inEditMode then
                let
                    ( keys, cmd ) =
                        Keyboard.Combo.update msg model.keys
                in
                { model | keys = keys } ! [ cmd ]
            else
                model ! []

        Undo ->
            { model
                | blockState = UndoList.undo model.blockState
                , viewAltRefs = False
            }
                ! []

        Redo ->
            { model
                | blockState = UndoList.redo model.blockState
                , viewAltRefs = False
            }
                ! []

        ToggleHelp ->
            { model | inHelp = not model.inHelp } ! []

        ListRefsByType mayRefType ->
            let
                newModel =
                    if mayRefType == model.selectedRefType then
                        { model | selectedRefType = Nothing }
                    else
                        { model | selectedRefType = mayRefType }

                listedRefArray =
                    getListedRefArray newModel
            in
            { newModel | listedRefIds = listedRefArray } ! []

        ToDoc dir ->
            let
                selectedDoc =
                    getNearbyDocId dir model
            in
            if selectedDoc == model.currentDocId then
                model ! []
            else
                let
                    newModel =
                        { model
                            | currentDocId = selectedDoc
                            , selectedRefType = Nothing
                            , viewAltRefs = False
                            , viewScriptureText = False
                            , scriptureText = ""
                        }

                    docRefArray =
                        getDocRefArray newModel
                in
                { newModel
                    | docRefIds = docRefArray
                    , listedRefIds = docRefArray
                }
                    ! [ backtoTop True ]

        ToRef dir mayConfirm ->
            let
                selectedRef =
                    case mayConfirm of
                        Nothing ->
                            getNearbyRefId dir model.currentRefId model.listedRefIds

                        Just conf ->
                            case conf of
                                Unconfirmed ->
                                    getNearbyUnconfId dir model.currentRefId model.listedRefIds

                                _ ->
                                    model.currentRefId

                refListId =
                    getRefListId selectedRef

                blockId =
                    getBlockIdFromRefId selectedRef
            in
            if model.currentRefId == selectedRef then
                model ! []
            else
                { model
                    | currentRefId = selectedRef
                    , viewAltRefs = False
                    , viewScriptureText = False
                    , scriptureText = ""
                }
                    ! [ scrollList refListId, scrollDoc blockId ]

        HandleBlockRefClick refId ->
            let
                isGoodRef =
                    isInRefIdArray refId model.docRefIds

                isListedRef =
                    isInRefIdArray refId model.listedRefIds

                newModel =
                    if isGoodRef && isListedRef then
                        model
                    else
                        { model | selectedRefType = Nothing }

                listedRefArray =
                    getListedRefArray newModel

                refListId =
                    getRefListId refId
            in
            if isGoodRef then
                { newModel
                    | currentRefId = refId
                    , viewAltRefs = False
                    , viewScriptureText = False
                    , scriptureText = ""
                }
                    ! [ scrollList refListId ]
            else
                model ! []

        HandleListRefClick refId ->
            let
                blockId =
                    getBlockIdFromRefId refId
            in
            if model.currentRefId == refId then
                model ! []
            else
                { model
                    | currentRefId = refId
                    , viewAltRefs = False
                    , viewScriptureText = False
                    , scriptureText = ""
                }
                    ! [ scrollDoc blockId ]

        ToggleAltRefs ->
            { model | viewAltRefs = not model.viewAltRefs } ! []

        SetScripText (Err err) ->
            let
                _ =
                    Debug.log "Err" err
            in
            model ! []

        SetScripText (Ok html) ->
            { model | scriptureText = html } ! []

        ShowScripture osis ->
            { model | viewScriptureText = True } ! [ fetchScripText osis ]

        HandlePostResponse (Err err) ->
            let
                _ =
                    Debug.log "Err" err
            in
            { model | isSaving = False } ! []

        HandlePostResponse (Ok html) ->
            { model | isSaving = False } ! []

        ConfirmRef ->
            let
                
            in

            { model | }

        ChangeOsis osis ->
            let
                blockId =
                    getBlockIdFromRefId model.currentRefId

                block =
                    Dict.get blockId model.blockState.present

                ref =
                    case block of
                        Nothing ->
                            Nothing

                        Just block ->
                            Dict.get model.currentRefId block.refs
            in
            case block of
                Nothing ->
                    model ! []

                Just block ->
                    {- let
                           newBlock =
                               getBlockWithNewRef osis model.currentRefId block

                           newBlockDict =
                               Dict.update blockId (always (Just newBlock)) model.blockState

                           newBlockState =
                               UndoList.new newBlockDict model.blockState
                       in
                    -}
                    model ! []
