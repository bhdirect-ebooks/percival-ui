port module Update exposing (update)

import Dict
import ServerIO exposing (fetchScripText, postBlock)
import Set
import Types exposing (..)
import UndoList exposing (UndoList)
import Update.Context exposing (addContextToBlock, editContext, updateContextField)
import Update.Editor
    exposing
        ( cancelHtml
        , editBlock
        , handleErrorMessage
        , handleHtmlError
        , handleHtmlSuccess
        , handleSuccessMessage
        , revertHtml
        , submitHtml
        , toggleEditorTheme
        , updateSource
        )
import Update.KeyCombo exposing (comboMsg)
import Update.ListRefs exposing (listRefs)
import Update.Load exposing (load, loadErr)
import Update.Navigate exposing (toDoc, toDocFromDash, toRef)
import Update.OsisField exposing (changeOsis, handleParserError, handleParserSuccess, updateField)
import Update.Redo exposing (redo)
import Update.SelectRefs exposing (clearMultiSelect, handleBlockRefClick, handleListRefClick, handleMultiSelect)
import Update.Undo exposing (undo)
import Utils exposing (..)


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
            load data model

        LoadData (Err err) ->
            loadErr err model

        ComboMsg kbcMsg ->
            comboMsg kbcMsg model

        Undo ->
            undo model

        Redo ->
            redo model

        ToggleDash ->
            { model | showDash = not model.showDash } ! []

        ToggleHelp ->
            { model | inHelp = not model.inHelp } ! []

        ListRefsByType mayRefType ->
            listRefs mayRefType model

        ToDocFromDash docId ->
            toDocFromDash docId model

        ToDoc navData ->
            toDoc navData model

        ToRef dir mayConfirm ->
            toRef dir mayConfirm model

        HandleBlockRefClick refId ->
            handleBlockRefClick refId model

        HandleMultiSelect refIdList ->
            handleMultiSelect refIdList model

        ClearSelected ->
            clearMultiSelect model

        HandleListRefClick refId ->
            handleListRefClick refId model

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

        HideScripture ->
            { model | viewScriptureText = False } ! []

        HandlePostResponse (Err err) ->
            let
                _ =
                    Debug.log "Err" err
            in
            { model | isSaving = False } ! []

        HandlePostResponse (Ok _) ->
            { model | isSaving = False } ! []

        ChangeRefData refDP ->
            if List.length model.selectedRefIds > 1 then
                changeMultipleRefs refDP model

            else
                changeSingleRef refDP model

        EditOsis ->
            changeOsis model

        UpdateField str ->
            updateField str model

        ChangeOsis ->
            changeOsis model

        HandleParserResponse (Err err) ->
            handleParserError err model

        HandleParserResponse (Ok refData) ->
            let
                ( newModel, cmd ) =
                    chooseParsedPath refData model
            in
            handleParserSuccess refData ( newModel, cmd )

        EditContext state ->
            editContext state model

        UpdateContextField str ->
            updateContextField str model

        AddContextToBlock ->
            addContextToBlock model

        EditBlock blockId ->
            editBlock blockId model

        UpdateSource str ->
            updateSource str model

        ToggleEditorTheme ->
            toggleEditorTheme model

        CancelHtml ->
            cancelHtml model

        RevertHtml ->
            revertHtml model

        SubmitHtml ->
            submitHtml model

        HandleMessages (Err err) ->
            handleErrorMessage err model

        HandleMessages (Ok res) ->
            let
                msgList =
                    List.map (\r -> r.message) res.messages
            in
            handleSuccessMessage msgList model

        HandlePostHtml (Err err) ->
            handleHtmlError err model

        HandlePostHtml (Ok block) ->
            handleHtmlSuccess block model


chooseParsedPath : RefData -> Model -> ( Model, Cmd Msg )
chooseParsedPath refData model =
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


changeSingleRef : RefDataPoint -> Model -> ( Model, Cmd Msg )
changeSingleRef refDP model =
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

                interimModel =
                    { model
                        | blockState = newBlockState
                        , isSaving = True
                    }

                ( newModel, cmd ) =
                    if (refDP == UserConf Confirmed) || (refDP == Remove) then
                        update (ToRef Next (Just Unconfirmed)) interimModel

                    else
                        ( { interimModel
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


updateStateForMultipleBlocks : List ( String, Block ) -> UndoList State -> UndoList State
updateStateForMultipleBlocks newBlockTups blockState =
    case newBlockTups of
        [] ->
            blockState

        ( blockId, block ) :: tail ->
            let
                newBlockDict =
                    blockState.present.blocks
                        |> Dict.update blockId (always (Just block))

                newBlockState =
                    blockState
                        |> UndoList.new
                            { changedBlockId = blockId
                            , blocks = newBlockDict
                            }
            in
            updateStateForMultipleBlocks tail newBlockState


prepMultiplePostCommands : List ( String, Block ) -> Cmd Msg -> Cmd Msg
prepMultiplePostCommands newBlockTups cmd =
    case newBlockTups of
        [] ->
            cmd

        ( blockId, block ) :: tail ->
            [ postBlock blockId block ]
                |> List.append [ cmd ]
                |> Cmd.batch
                |> prepMultiplePostCommands tail


changeMultipleRefs : RefDataPoint -> Model -> ( Model, Cmd Msg )
changeMultipleRefs refDP model =
    let
        -- 1. get unique list of blockIds
        blockIds =
            model.selectedRefIds
                |> List.map getBlockIdFromRefId
                |> Set.fromList
                |> Set.toList

        -- 2. get all blocks
        blockTups =
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

        -- 3. update all block refs in each block
        newBlockTups =
            blockTups
                |> List.map
                    (\( blockId, block ) ->
                        ( blockId, updateListofBlockRefs model.selectedRefIds refDP blockId block )
                    )

        -- 4. create new block dict (update each block)
        -- 5. update state for each block
        newBlockState =
            updateStateForMultipleBlocks newBlockTups model.blockState

        interimModel =
            { model
                | blockState = newBlockState
                , isSaving = True
            }

        -- 6. set interim model
        ( newModel, cmd ) =
            update ClearSelected interimModel

        -- 7. prep commands
        batch =
            prepMultiplePostCommands newBlockTups cmd

        -- 8. update dashboard
        repBlockId =
            case blockIds of
                [] ->
                    ""

                head :: _ ->
                    head

        dashboard =
            updateDashboard repBlockId newModel
    in
    { newModel | dashboard = dashboard } ! [ batch ]
