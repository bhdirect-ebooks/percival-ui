port module Update exposing (..)

import Dict exposing (..)
import Dom
import Keyboard.Combo
import Regex
import ServerIO exposing (fetchScripText, postBlock, postContext, postFieldInput, postNewHtml, postValidateHtml)
import Task
import Types exposing (..)
import UndoList exposing (UndoList)
import Utils exposing (..)


port clickedRef : (String -> msg) -> Sub msg


port scrollList : String -> Cmd msg


port scrollDoc : String -> Cmd msg


port backtoTop : Bool -> Cmd msg


keyboardCombos : List (Keyboard.Combo.KeyCombo Msg)
keyboardCombos =
    [ Keyboard.Combo.combo1 Keyboard.Combo.q (ToDoc (ByDir Prev))
    , Keyboard.Combo.combo1 Keyboard.Combo.w (ToDoc (ByDir Next))
    , Keyboard.Combo.combo1 Keyboard.Combo.up (ToRef Prev Nothing)
    , Keyboard.Combo.combo1 Keyboard.Combo.down (ToRef Next Nothing)
    , Keyboard.Combo.combo1 Keyboard.Combo.left (ToRef Prev (Just Unconfirmed))
    , Keyboard.Combo.combo1 Keyboard.Combo.right (ToRef Next (Just Unconfirmed))
    , Keyboard.Combo.combo1 Keyboard.Combo.escape (ListRefsByType Nothing)
    , Keyboard.Combo.combo1 Keyboard.Combo.c (ChangeRefData (UserConf Confirmed))
    , Keyboard.Combo.combo1 Keyboard.Combo.u Undo
    , Keyboard.Combo.combo1 Keyboard.Combo.i Redo
    , Keyboard.Combo.combo1 Keyboard.Combo.backspace (ChangeRefData Remove)
    , Keyboard.Combo.combo1 Keyboard.Combo.h ToggleHelp
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
                        | volumeTitle = data.volumeTitle
                        , parserOpts = data.parserOpts
                        , docs = data.docs
                        , blockState =
                            UndoList.fresh
                                { changedBlockId = ""
                                , blocks = data.blocks
                                }
                        , currentDocId = getFirstIdOfDict data.docs
                    }

                docRefArray =
                    getDocRefArray newModel

                dashboard =
                    getDashboard newModel data.trueDocs
            in
            { newModel
                | docRefIds = docRefArray
                , listedRefIds = docRefArray
                , dashboard = dashboard
            }
                ! []

        LoadData (Err err) ->
            let
                _ =
                    Debug.log "Err" err

                showError =
                    Regex.replace (Regex.AtMost 1)
                        (Regex.regex "^(.*?), body = .*?$")
                        (\{ submatches } ->
                            case submatches of
                                first :: _ ->
                                    Maybe.withDefault "" first

                                _ ->
                                    ""
                        )
                        (toString err)
            in
            { model | loadingError = Just showError } ! []

        ComboMsg msg ->
            let
                ( keys, cmd ) =
                    Keyboard.Combo.update msg model.keys
            in
            { model | keys = keys } ! [ cmd ]

        Undo ->
            if UndoList.hasPast model.blockState && not model.inEditMode then
                let
                    newState =
                        UndoList.undo model.blockState

                    blockId =
                        model.blockState.present.changedBlockId

                    block =
                        Dict.get blockId newState.present.blocks

                    osisOrMessage =
                        getOsisWithRefId model.currentRefId model.blockState.present.blocks

                    newModel =
                        { model | blockState = newState }

                    dashboard =
                        updateDashboard blockId newModel
                in
                case block of
                    Nothing ->
                        { newModel
                            | dashboard = dashboard
                            , viewAltRefs = False
                        }
                            ! []

                    Just block ->
                        { newModel
                            | dashboard = dashboard
                            , viewAltRefs = False
                            , isSaving = True
                            , viewScriptureText = False
                            , scriptureText = ""
                            , osisField = osisOrMessage
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

                    osisOrMessage =
                        getOsisWithRefId model.currentRefId model.blockState.present.blocks

                    newModel =
                        { model | blockState = newState }

                    dashboard =
                        updateDashboard blockId newModel
                in
                case block of
                    Nothing ->
                        { newModel
                            | dashboard = dashboard
                            , viewAltRefs = False
                        }
                            ! []

                    Just block ->
                        { newModel
                            | dashboard = dashboard
                            , viewAltRefs = False
                            , isSaving = True
                            , viewScriptureText = False
                            , scriptureText = ""
                            , osisField = osisOrMessage
                        }
                            ! [ postBlock blockId block ]
            else
                model ! []

        ToggleDash ->
            { model | showDash = not model.showDash } ! []

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

        ToDocFromDash docId ->
            { model | showDash = False } ! [ Task.succeed (ToDoc (ById docId)) |> Task.perform identity ]

        ToDoc navData ->
            let
                selectedDoc =
                    case navData of
                        ByDir dir ->
                            getNearbyDocId dir model

                        ById docId ->
                            docId
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
                    ! [ scrollList refListId, scrollDoc selectedRef ]

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
                    | listedRefIds = listedRefArray
                    , currentRefId = refId
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
                    ! [ scrollDoc refId ]

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

        HandlePostResponse (Ok _) ->
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

                        iterimModel =
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

                        ( newModel, cmd ) =
                            if refDP == UserConf Confirmed then
                                update (ToRef Next (Just Unconfirmed)) iterimModel
                            else
                                ( { iterimModel
                                    | viewScriptureText = False
                                    , scriptureText = ""
                                  }
                                , Cmd.none
                                )

                        osisOrMessage =
                            getOsisWithRefId newModel.currentRefId newModel.blockState.present.blocks

                        commands =
                            [ cmd ] |> List.append [ postBlock blockId newBlock ]

                        dashboard =
                            updateDashboard blockId newModel
                    in
                    { newModel
                        | dashboard = dashboard
                        , osisField = osisOrMessage
                    }
                        ! commands

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
                { model | badInput = True } ! [ Task.attempt (\_ -> DoNothing) (Dom.focus "osis-field") ]
            else if not (model.osisField == currentOsisOrMess) then
                { model
                    | inEditMode = False
                    , editingOsis = False
                    , badInput = False
                    , viewScriptureText = False
                    , scriptureText = ""
                }
                    ! [ Task.attempt (\_ -> DoNothing) (Dom.blur "osis-field")
                      , postFieldInput model.osisField model.parserOpts
                      ]
            else
                { model
                    | inEditMode = False
                    , editingOsis = False
                    , badInput = False
                }
                    ! [ Task.attempt (\_ -> DoNothing) (Dom.blur "osis-field") ]

        HandleParserResponse (Err err) ->
            let
                _ =
                    Debug.log "Err" err
            in
            { model | badInput = True } ! [ Task.attempt (\_ -> DoNothing) (Dom.focus "osis-field") ]

        HandleParserResponse (Ok refData) ->
            let
                ( newModel, cmd ) =
                    if refData.valid then
                        update (ChangeRefData (Scripture refData.scripture))
                            { model
                                | badInput = False
                                , inEditMode = False
                                , editingOsis = False
                                , osisField = refData.scripture
                            }
                    else
                        update (ChangeRefData (UserVal Invalid refData.message))
                            { model
                                | badInput = True
                                , inEditMode = True
                                , editingOsis = True
                                , osisField = refData.message
                            }

                domCmd =
                    if refData.valid then
                        Dom.blur "osis-field"
                    else
                        Dom.focus "osis-field"

                commands =
                    [ cmd ]
                        |> List.append [ Task.attempt (\_ -> DoNothing) domCmd ]
            in
            newModel ! commands

        EditContext state ->
            { model
                | inEditMode = state
                , editingContext = state
            }
                ! []

        UpdateContextField str ->
            { model | contextField = str } ! []

        AddContextToBlock ->
            let
                blockId =
                    getBlockIdFromRefId model.currentRefId

                blurTask =
                    Task.attempt (\_ -> DoNothing) (Dom.blur "context-field")
            in
            if not (model.contextField == "") then
                { model
                    | contextField = ""
                    , isSaving = True
                    , editingBlockId = blockId
                }
                    ! [ postContext blockId model.contextField model.parserOpts, blurTask ]
            else
                model ! [ blurTask ]

        EditBlock blockId ->
            let
                block =
                    Dict.get blockId model.blockState.present.blocks
            in
            case block of
                Nothing ->
                    model ! []

                Just block ->
                    { model
                        | htmlSource = block.html
                        , inEditMode = True
                        , editorActive = True
                        , editingBlockId = blockId
                    }
                        ! []

        UpdateSource str ->
            { model | htmlSource = str } ! []

        ToggleEditorTheme ->
            let
                newTheme =
                    case model.editorTheme of
                        Dark ->
                            Light

                        Light ->
                            Dark
            in
            { model | editorTheme = newTheme } ! []

        CancelHtml ->
            { model
                | htmlSource = ""
                , inEditMode = False
                , editorActive = False
                , editingBlockId = ""
                , isValidating = False
                , htmlValidation = []
            }
                ! []

        RevertHtml ->
            let
                block =
                    Dict.get model.editingBlockId model.blockState.present.blocks
            in
            case block of
                Nothing ->
                    model ! []

                Just block ->
                    { model
                        | htmlSource = block.html
                        , isValidating = False
                        , htmlValidation = []
                    }
                        ! []

        SubmitHtml ->
            if model.htmlSource == "" then
                { model | htmlValidation = [ "Cannot be empty." ] }
                    ! [ Task.attempt (\_ -> DoNothing) (Dom.focus model.editingBlockId) ]
            else
                { model | isValidating = True } ! [ postValidateHtml model.htmlSource ]

        HandleMessages (Err err) ->
            let
                _ =
                    Debug.log "Err" err
            in
            if toString err == "NetworkError" then
                { model
                    | htmlValidation = []
                    , isValidating = False
                    , isSaving = True
                }
                    ! [ postNewHtml model.editingBlockId model.htmlSource ]
            else
                { model
                    | htmlValidation = [ toString err ]
                    , isValidating = False
                }
                    ! [ Task.attempt (\_ -> DoNothing) (Dom.focus model.editingBlockId) ]

        HandleMessages (Ok res) ->
            let
                msgList =
                    List.map (\r -> r.message) res.messages

                newModel =
                    { model
                        | htmlValidation = msgList
                        , isValidating = False
                    }
            in
            if List.isEmpty msgList then
                { newModel | isSaving = True } ! [ postNewHtml model.editingBlockId model.htmlSource ]
            else
                newModel ! [ Task.attempt (\_ -> DoNothing) (Dom.focus model.editingBlockId) ]

        HandlePostHtml (Err err) ->
            let
                _ =
                    Debug.log "Err" err
            in
            { model
                | htmlValidation = [ toString err ]
                , isSaving = False
            }
                ! [ Task.attempt (\_ -> DoNothing) (Dom.focus model.editingBlockId) ]

        HandlePostHtml (Ok block) ->
            let
                newBlockDict =
                    model.blockState.present.blocks
                        |> Dict.update model.editingBlockId (always (Just block))

                newBlockState =
                    model.blockState
                        |> UndoList.new
                            { changedBlockId = model.editingBlockId
                            , blocks = newBlockDict
                            }

                osisOrMessage =
                    getOsisWithRefId model.currentRefId newBlockState.present.blocks

                interimModel =
                    { model
                        | htmlSource = ""
                        , inEditMode = False
                        , editorActive = False
                        , editingBlockId = ""
                        , blockState = newBlockState
                        , isSaving = False
                        , viewAltRefs = False
                        , viewScriptureText = False
                        , scriptureText = ""
                        , osisField = osisOrMessage
                    }

                docRefArray =
                    getDocRefArray interimModel

                listedRefArray =
                    getListedRefArray interimModel

                newModel =
                    { interimModel
                        | docRefIds = docRefArray
                        , listedRefIds = listedRefArray
                    }

                dashboard =
                    updateDashboard model.editingBlockId newModel
            in
            { newModel | dashboard = dashboard } ! []
