module Main exposing
    ( initialCmd
    , initialModel
    , main
    , subscriptions
    )

import Array
import Dict
import Html
import Http
import Keyboard.Combo
import ServerIO exposing (decodePercivalData)
import Types exposing (..)
import UndoList exposing (UndoList)
import Update exposing (update)
import Update.KeyCombo exposing (keyboardCombos)
import Update.SelectRefs exposing (clickedRef, multiSelect)
import Update.SelectText exposing (textSelected)
import View exposing (viewOrError)


initialModel : Model
initialModel =
    { volumeTitle = ""
    , parserOpts =
        { versification = ""
        , language = ""
        }
    , dashboard =
        { totals =
            { confirmed = 0
            , lowConfidence = 0
            , invalid = 0
            }
        , docStats = []
        }
    , showDash = True
    , docs = Dict.fromList []
    , blockState =
        UndoList.fresh
            { changedBlockIds = []
            , blocks = Dict.fromList []
            }
    , currentDocId = ""
    , currentRefId = ""
    , selectedRefIds = []
    , editingOsis = False
    , osisField = ""
    , badInput = False
    , editingContext = False
    , contextField = ""
    , htmlSource = ""
    , editingBlockId = ""
    , editorActive = False
    , editorTheme = Dark
    , isValidating = False
    , htmlValidation = []
    , loadingError = Nothing
    , isSaving = False
    , inEditMode = False
    , inHelp = False
    , docRefIds = Array.fromList []
    , selectedRefType = Nothing
    , listedRefIds = Array.fromList []
    , keys = Keyboard.Combo.init keyboardCombos ComboMsg
    , viewAltRefs = False
    , viewScriptureText = False
    , scriptureText = ""
    , selection =
        { blockId = ""
        , selectedText = ""
        , anchorOffset = 0
        , focusOffset = 0
        , textContent = ""
        }
    }


initialCmd : Cmd Msg
initialCmd =
    decodePercivalData
        |> Http.get "http://localhost:7777/data/"
        |> Http.send LoadData


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.inEditMode then
        Sub.batch
            [ clickedRef HandleBlockRefClick
            , multiSelect HandleMultiSelect
            ]

    else
        Sub.batch
            [ Keyboard.Combo.subscriptions model.keys
            , clickedRef HandleBlockRefClick
            , multiSelect HandleMultiSelect
            , textSelected HandleTextSelection
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initialCmd )
        , view = viewOrError
        , update = update
        , subscriptions = subscriptions
        }
