module Update exposing (..)

import Array
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
            { model | blockState = UndoList.undo model.blockState } ! []

        Redo ->
            { model | blockState = UndoList.redo model.blockState } ! []

        ToggleHelp ->
            { model | inHelp = not model.inHelp } ! []

        ListRefsByType mayRefType ->
            let
                newModel =
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
                { model | currentRefId = selectedRef } ! [ scrollList refListId, scrollDoc blockId ]

        HandleBlockRefClick refId ->
            let
                isGoodRef =
                    model.docRefIds
                        |> Array.toList
                        |> List.filter (\tup -> Tuple.first tup == refId)
                        |> List.isEmpty
                        |> not

                newModel =
                    { model | selectedRefType = Nothing }

                listedRefArray =
                    getListedRefArray newModel

                refListId =
                    getRefListId refId
            in
            if isGoodRef then
                { newModel | currentRefId = refId } ! [ scrollList refListId ]
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
                { model | currentRefId = refId } ! [ scrollDoc blockId ]
