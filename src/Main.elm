module Main exposing (..)

import Array
import Decoders exposing (decodePercivalData)
import Dict
import Html
import Http
import Keyboard.Combo
import Types exposing (..)
import UndoList exposing (UndoList)
import Update exposing (..)
import Utils exposing (clickedRef)
import View exposing (viewOrError)


initialModel : Model
initialModel =
    { percivalData =
        { volumeTitle = ""
        , parserOpts =
            { versification = ""
            , language = ""
            }
        , docs = Dict.fromList []
        , blocks = Dict.fromList []
        }
    , blockState =
        UndoList.fresh
            { changedBlockId = ""
            , blocks = Dict.fromList []
            }
    , currentDocId = ""
    , currentRefId = ""
    , editingOsis = False
    , osisField = ""
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
