module Update exposing (..)

import Keyboard.Combo
import Types exposing (..)
import UndoList exposing (UndoList)
import Utils exposing (..)


keyboardCombos : List (Keyboard.Combo.KeyCombo Msg)
keyboardCombos =
    [ Keyboard.Combo.combo1 Keyboard.Combo.q ToPrevDoc
    , Keyboard.Combo.combo1 Keyboard.Combo.w ToNextDoc
    , Keyboard.Combo.combo1 Keyboard.Combo.h ToggleHelp
    , Keyboard.Combo.combo1 Keyboard.Combo.up ToPrevRef
    , Keyboard.Combo.combo1 Keyboard.Combo.down ToNextRef
    , Keyboard.Combo.combo1 Keyboard.Combo.left ToPrevUnconf
    , Keyboard.Combo.combo1 Keyboard.Combo.right ToNextUnconf
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        LoadData (Ok data) ->
            ( { model
                | percivalData = data
                , blockState = UndoList.fresh data.blocks
                , currentDocId = getFirstIdOfDict data.docs
              }
            , Cmd.none
            )

        LoadData (Err err) ->
            let
                _ =
                    Debug.log "Err" err
            in
            ( { model
                | loadingError = Just "Something went wrong."
              }
            , Cmd.none
            )

        Undo ->
            ( { model | blockState = UndoList.undo model.blockState }, Cmd.none )

        Redo ->
            ( { model | blockState = UndoList.redo model.blockState }, Cmd.none )

        ToNextDoc ->
            ( { model | currentDocId = getNextDocId model }, Cmd.none )

        ToPrevDoc ->
            ( { model | currentDocId = getPrevDocId model }, Cmd.none )

        ToggleHelp ->
            ( { model | inHelp = not model.inHelp }, Cmd.none )

        ComboMsg msg ->
            if not model.inEditMode then
                let
                    ( keys, cmd ) =
                        Keyboard.Combo.update msg model.keys
                in
                ( { model | keys = keys }, cmd )
            else
                ( model, Cmd.none )

        ShowRefs mayRefType ->
            ( { model | listedRefs = mayRefType }, Cmd.none )
