module Update.KeyCombo exposing (comboMsg, keyboardCombos)

import Keyboard.Combo
import Types exposing (..)


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
    , Keyboard.Combo.combo1 Keyboard.Combo.d ToggleDash
    ]


comboMsg : Keyboard.Combo.Msg -> Model -> ( Model, Cmd Msg )
comboMsg kbcMsg model =
    let
        ( keys, cmd ) =
            Keyboard.Combo.update kbcMsg model.keys
    in
    { model | keys = keys } ! [ cmd ]
