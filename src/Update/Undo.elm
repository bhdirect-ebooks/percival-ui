module Update.Undo exposing (undo)

import Dict exposing (..)
import ServerIO exposing (postBlock)
import Types exposing (..)
import UndoList exposing (UndoList)
import Utils exposing (..)


undo : Model -> ( Model, Cmd Msg )
undo model =
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
