module Main exposing (..)

import Decoders exposing (decodePercivalData)
import Dict
import Html
import Http
import Keyboard.Combo
import Types exposing (..)
import UndoList exposing (UndoList)
import Update exposing (..)
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
    , blockState = UndoList.fresh (Dict.fromList [])
    , currentDocId = ""
    , currentRefId = ""
    , loadingError = Nothing
    , isSaving = False
    , inEditMode = False
    , inHelp = False
    , listedRefs = Nothing
    , keys = Keyboard.Combo.init keyboardCombos ComboMsg
    }


initialCmd : Cmd Msg
initialCmd =
    decodePercivalData
        |> Http.get "http://localhost:7777/data/"
        |> Http.send LoadData


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.Combo.subscriptions model.keys


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initialCmd )
        , view = viewOrError
        , update = update
        , subscriptions = subscriptions
        }
