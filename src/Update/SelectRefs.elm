port module Update.SelectRefs exposing
    ( clearMultiSelect
    , clearSelected
    , clickedRef
    , handleBlockRefClick
    , handleListRefClick
    , handleMultiSelect
    , multiSelect
    )

import Task
import Types exposing (..)
import Update.Navigate exposing (scrollDoc, scrollList)
import Update.SelectText exposing (clearTextSelection)
import Utils exposing (..)


port clearSelected : Bool -> Cmd msg


port clickedRef : (String -> msg) -> Sub msg


port multiSelect : (List String -> msg) -> Sub msg


handleBlockRefClick : RefId -> Model -> ( Model, Cmd Msg )
handleBlockRefClick refId model =
    let
        isGoodRef =
            isInRefIdArray refId model.docRefIds

        isListedRef =
            isInRefIdArray refId model.listedRefIds

        interimModel =
            if isGoodRef && isListedRef then
                model

            else
                { model | selectedRefType = Nothing }

        newModel =
            clearTextSelection interimModel

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


handleMultiSelect : List RefId -> Model -> ( Model, Cmd Msg )
handleMultiSelect refIdList model =
    let
        newModel =
            { model
                | currentRefId = ""
                , selectedRefIds = refIdList
                , viewAltRefs = False
                , viewScriptureText = False
                , scriptureText = ""
                , osisField = ""
            }
    in
    case refIdList of
        [] ->
            { newModel | listedRefIds = model.docRefIds } ! [ clearSelected True ]

        [ refId ] ->
            newModel ! [ Task.succeed (HandleBlockRefClick refId) |> Task.perform identity ]

        _ ->
            let
                listedRefArray =
                    getListedRefArrayBySelected newModel
            in
            { newModel | listedRefIds = listedRefArray } ! []


clearMultiSelect : Model -> ( Model, Cmd Msg )
clearMultiSelect model =
    handleMultiSelect [] model


handleListRefClick : RefId -> Model -> ( Model, Cmd Msg )
handleListRefClick refId model =
    let
        blockId =
            getBlockIdFromRefId refId

        osisOrMessage =
            getOsisWithRefId refId model.blockState.present.blocks

        newModel =
            clearTextSelection model
    in
    if model.currentRefId == refId || model.inEditMode then
        model ! []

    else
        { newModel
            | currentRefId = refId
            , viewAltRefs = False
            , viewScriptureText = False
            , scriptureText = ""
            , osisField = osisOrMessage
        }
            ! [ scrollDoc refId ]
