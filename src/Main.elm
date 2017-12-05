module Main exposing (..)

import Array
import Dict
import Html
import Http
import Keyboard.Combo
import ServerIO exposing (decodePercivalData)
import Types exposing (..)
import Update exposing (..)
import View exposing (viewOrError)


initialModel : Model
initialModel =
    { loadingError = Nothing
    , keys = Keyboard.Combo.init keyboardCombos ComboMsg
    , isSaving = False
    , inEditMode = False
    , inHelp = False
    , undoList =
        { past = []
        , future = []
        }
    , volumeTitle = ""
    , parserOpts =
        { versification = ""
        , language = ""
        }
    }
    , docs =
        { prev : List DocId
        , current : DocData
        , next : List DocId
        }
    , editor =
        { htmlSource = ""
        , editingBlockId = ""
        , editorActive = False
        , editorTheme = Dark
        , isValidating = False
        , htmlValidation = []
        }
    , scriptureList =
        { selectedRefType = Nothing
        , docRefIds = []
        , listedRefIds =
            { prev : []
            , current : ("", Unconfirmed)
            , next : []
            }
        }
    , actionPanel =
        { editingOsis = False
        , osisField = ""
        , badInput = False
        , editingContext = False
        , contextField = ""
        , viewAltRefs = False
        , viewScriptureText = False
        , scriptureText = ""
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
