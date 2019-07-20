module Update.Context exposing (addContextToBlock, editContext, updateContextField)

import Dom
import ServerIO exposing (postContext)
import Task
import Types exposing (..)
import Utils exposing (..)


editContext : Bool -> Model -> ( Model, Cmd Msg )
editContext state model =
    { model
        | inEditMode = state
        , editingContext = state
    }
        ! []


updateContextField : String -> Model -> ( Model, Cmd Msg )
updateContextField str model =
    { model | contextField = str } ! []


addContextToBlock : Model -> ( Model, Cmd Msg )
addContextToBlock model =
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
