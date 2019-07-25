module Update.UndoRedo exposing (redo, undo)

import Dict exposing (..)
import ServerIO exposing (prepMultiplePostCommands)
import Types exposing (..)
import UndoList exposing (UndoList)
import Utils exposing (..)


processUndoRedo : UndoList State -> List String -> Model -> ( Model, Cmd Msg )
processUndoRedo newState blockIds model =
    let
        blocks =
            blockIds
                |> List.map
                    (\blockId -> Dict.get blockId newState.present.blocks)

        osisOrMessage =
            getOsisWithRefId model.currentRefId model.blockState.present.blocks

        interimModel =
            { model | blockState = newState }

        repBlockId =
            case blockIds of
                [] ->
                    ""

                head :: _ ->
                    head

        -- updateDashboard actually works by docId,
        -- so we need only a representative blockId
        dashboard =
            updateDashboard repBlockId interimModel

        newModel =
            { interimModel
                | dashboard = dashboard
                , viewAltRefs = False
                , isSaving = True
                , viewScriptureText = False
                , scriptureText = ""
                , osisField = osisOrMessage
            }

        newBlockTups =
            getListofBlockTups blockIds newModel

        batch =
            prepMultiplePostCommands newBlockTups Cmd.none
    in
    newModel ! [ batch ]


undo : Model -> ( Model, Cmd Msg )
undo model =
    if UndoList.hasPast model.blockState && not model.inEditMode then
        let
            newState =
                UndoList.undo model.blockState

            blockIds =
                model.blockState.present.changedBlockIds
        in
        processUndoRedo newState blockIds model

    else
        model ! []


redo : Model -> ( Model, Cmd Msg )
redo model =
    if UndoList.hasFuture model.blockState && not model.inEditMode then
        let
            newState =
                UndoList.redo model.blockState

            blockIds =
                newState.present.changedBlockIds
        in
        processUndoRedo newState blockIds model

    else
        model ! []
