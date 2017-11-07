module Update exposing (..)

import Dict exposing (..)
import Dom
import Keyboard.Combo
import Task
import Types exposing (..)
import UndoList exposing (UndoList)
import Utils exposing (..)


keyboardCombos : List (Keyboard.Combo.KeyCombo Msg)
keyboardCombos =
    [ Keyboard.Combo.combo1 Keyboard.Combo.q (ToDoc Prev)
    , Keyboard.Combo.combo1 Keyboard.Combo.w (ToDoc Next)
    , Keyboard.Combo.combo1 Keyboard.Combo.up (ToRef Prev Nothing)
    , Keyboard.Combo.combo1 Keyboard.Combo.down (ToRef Next Nothing)
    , Keyboard.Combo.combo1 Keyboard.Combo.left (ToRef Prev (Just Unconfirmed))
    , Keyboard.Combo.combo1 Keyboard.Combo.right (ToRef Next (Just Unconfirmed))
    , Keyboard.Combo.combo1 Keyboard.Combo.escape (ListRefsByType Nothing)
    , Keyboard.Combo.combo1 Keyboard.Combo.enter (ChangeRefData (UserConf Confirmed))
    , Keyboard.Combo.combo1 Keyboard.Combo.z Undo
    , Keyboard.Combo.combo2 ( Keyboard.Combo.shift, Keyboard.Combo.z ) Redo
    , Keyboard.Combo.combo2 ( Keyboard.Combo.shift, Keyboard.Combo.enter ) EditOsis
    , Keyboard.Combo.combo2 ( Keyboard.Combo.shift, Keyboard.Combo.r ) (ChangeRefData Remove)
    , Keyboard.Combo.combo2 ( Keyboard.Combo.shift, Keyboard.Combo.forwardSlash ) ToggleHelp
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
                        , blockState =
                            UndoList.fresh
                                { changedBlockId = ""
                                , blocks = data.blocks
                                }
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
            if UndoList.hasPast model.blockState && not model.inEditMode then
                let
                    newState =
                        UndoList.undo model.blockState

                    blockId =
                        model.blockState.present.changedBlockId

                    block =
                        Dict.get blockId newState.present.blocks
                in
                case block of
                    Nothing ->
                        { model
                            | blockState = newState
                            , viewAltRefs = False
                        }
                            ! []

                    Just block ->
                        { model
                            | blockState = newState
                            , viewAltRefs = False
                            , isSaving = True
                        }
                            ! [ postBlock blockId block ]
            else
                model ! []

        Redo ->
            if UndoList.hasFuture model.blockState && not model.inEditMode then
                let
                    newState =
                        UndoList.redo model.blockState

                    blockId =
                        newState.present.changedBlockId

                    block =
                        Dict.get blockId newState.present.blocks
                in
                case block of
                    Nothing ->
                        { model
                            | blockState = newState
                            , viewAltRefs = False
                        }
                            ! []

                    Just block ->
                        { model
                            | blockState = newState
                            , viewAltRefs = False
                            , isSaving = True
                        }
                            ! [ postBlock blockId block ]
            else
                model ! []

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
            if selectedDoc == model.currentDocId || model.inEditMode then
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

                osisOrMessage =
                    getOsisWithRefId selectedRef model.blockState.present.blocks
            in
            if model.currentRefId == selectedRef || model.inEditMode then
                model ! []
            else
                { model
                    | currentRefId = selectedRef
                    , viewAltRefs = False
                    , viewScriptureText = False
                    , scriptureText = ""
                    , osisField = osisOrMessage
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

                osisOrMessage =
                    getOsisWithRefId refId model.blockState.present.blocks
            in
            if isGoodRef && not model.inEditMode then
                { newModel
                    | currentRefId = refId
                    , viewAltRefs = False
                    , viewScriptureText = False
                    , scriptureText = ""
                    , osisField = osisOrMessage
                }
                    ! [ scrollList refListId ]
            else
                model ! []

        HandleListRefClick refId ->
            let
                blockId =
                    getBlockIdFromRefId refId

                osisOrMessage =
                    getOsisWithRefId refId model.blockState.present.blocks
            in
            if model.currentRefId == refId || model.inEditMode then
                model ! []
            else
                { model
                    | currentRefId = refId
                    , viewAltRefs = False
                    , viewScriptureText = False
                    , scriptureText = ""
                    , osisField = osisOrMessage
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

        ChangeRefData refDP ->
            let
                blockId =
                    getBlockIdFromRefId model.currentRefId

                block =
                    Dict.get blockId model.blockState.present.blocks
            in
            case block of
                Nothing ->
                    model ! []

                Just block ->
                    let
                        newBlock =
                            updateBlockRef model.currentRefId refDP block

                        newBlockDict =
                            model.blockState.present.blocks
                                |> Dict.update blockId (always (Just newBlock))

                        newBlockState =
                            model.blockState
                                |> UndoList.new
                                    { changedBlockId = blockId
                                    , blocks = newBlockDict
                                    }

                        newModel =
                            if refDP == Remove then
                                { model
                                    | blockState = newBlockState
                                    , isSaving = True
                                    , currentRefId = ""
                                }
                            else
                                { model
                                    | blockState = newBlockState
                                    , isSaving = True
                                }
                    in
                    newModel ! [ postBlock blockId newBlock ]

        EditOsis ->
            { model
                | inEditMode = True
                , editingOsis = True
            }
                ! [ Task.attempt (\_ -> DoNothing) (Dom.focus "osis-field") ]

        UpdateField str ->
            { model | osisField = str } ! []

        ChangeOsis ->
            let
                currentOsisOrMess =
                    getOsisWithRefId model.currentRefId model.blockState.present.blocks
            in
            if model.osisField == "" then
                model ! [ Task.attempt (\_ -> DoNothing) (Dom.focus "osis-field") ]
            else if not (model.osisField == currentOsisOrMess) then
                model ! []
            else
                { model
                    | inEditMode = False
                    , editingOsis = False
                }
                    ! [ Task.attempt (\_ -> DoNothing) (Dom.blur "osis-field") ]

        HandleParserResponse str ->
            model ! []
