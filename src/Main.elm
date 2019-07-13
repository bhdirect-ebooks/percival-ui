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
import Update exposing (..)
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
            { changedBlockId = ""
            , blocks = Dict.fromList []
            }
    , currentDocId = ""
    , currentRefId = ""
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
    }


initialCmd : Cmd Msg
initialCmd =
    decodePercivalData
        |> Http.get "http://localhost:7777/data/"
        |> Http.send LoadData


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.inEditMode then
        Sub.batch [ clickedRef HandleBlockRefClick ]

    else
        Sub.batch
            [ Keyboard.Combo.subscriptions model.keys
            , clickedRef HandleBlockRefClick
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initialCmd )
        , view = viewOrError
        , update = update
        , subscriptions = subscriptions
        }
