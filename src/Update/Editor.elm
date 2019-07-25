module Update.Editor exposing
    ( cancelHtml
    , editBlock
    , handleErrorMessage
    , handleHtmlError
    , handleHtmlSuccess
    , handleSuccessMessage
    , revertHtml
    , toggleEditorTheme
    , updateSource
    )

import Dict
import Dom
import ServerIO exposing (..)
import Task
import Types exposing (..)
import UndoList exposing (UndoList)
import Update.SelectRefs exposing (clearSelected)
import Update.SelectText exposing (clearTextSelection, deselect)
import Update.SubmitHtml exposing (submitHtml)
import Utils exposing (..)


editBlock : String -> Model -> ( Model, Cmd Msg )
editBlock blockId model =
    let
        block =
            Dict.get blockId model.blockState.present.blocks

        listedRefArray =
            getListedRefArray model

        newModel =
            clearTextSelection model
    in
    case block of
        Nothing ->
            model ! []

        Just block ->
            { newModel
                | currentRefId = ""
                , listedRefIds = listedRefArray
                , selectedRefIds = []
                , htmlSource = block.html
                , inEditMode = True
                , editorActive = True
                , editingBlockId = blockId
            }
                ! [ clearSelected True, deselect True ]


updateSource : String -> Model -> ( Model, Cmd Msg )
updateSource str model =
    { model | htmlSource = str } ! []


toggleEditorTheme : Model -> ( Model, Cmd Msg )
toggleEditorTheme model =
    let
        newTheme =
            case model.editorTheme of
                Dark ->
                    Light

                Light ->
                    Dark
    in
    { model | editorTheme = newTheme } ! []


cancelHtml : Model -> ( Model, Cmd Msg )
cancelHtml model =
    let
        newModel =
            clearTextSelection model
    in
    { newModel
        | htmlSource = ""
        , inEditMode = False
        , editorActive = False
        , editingBlockId = ""
        , isValidating = False
        , htmlValidation = []
    }
        ! [ deselect True ]


revertHtml : Model -> ( Model, Cmd Msg )
revertHtml model =
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


handleErrorMessage : a -> Model -> ( Model, Cmd Msg )
handleErrorMessage err model =
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


handleSuccessMessage : List String -> Model -> ( Model, Cmd Msg )
handleSuccessMessage msgList model =
    let
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


handleHtmlError : a -> Model -> ( Model, Cmd Msg )
handleHtmlError err model =
    let
        _ =
            Debug.log "Err" err
    in
    { model
        | htmlValidation = [ toString err ]
        , isSaving = False
    }
        ! [ Task.attempt (\_ -> DoNothing) (Dom.focus model.editingBlockId) ]


handleHtmlSuccess : Block -> Model -> ( Model, Cmd Msg )
handleHtmlSuccess block model =
    let
        newBlockDict =
            model.blockState.present.blocks
                |> Dict.update model.editingBlockId (always (Just block))

        newBlockState =
            model.blockState
                |> UndoList.new
                    { changedBlockIds = [ model.editingBlockId ]
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
                |> clearTextSelection

        dashboard =
            updateDashboard model.editingBlockId newModel
    in
    { newModel | dashboard = dashboard } ! []
