module Types exposing (..)

import Array
import Dict
import Dom
import Http
import Keyboard.Combo
import UndoList exposing (UndoList)


type EditorTheme
    = Dark
    | Light


type NavDir
    = Prev
    | Next


type Confidence
    = NotFull
    | Full


type Confirmation
    = Confirmed
    | Unconfirmed


type Validity
    = Valid
    | Invalid


type RefType
    = RefConf Confidence
    | RefVal Validity
    | Confirm Confirmation


type RefDataPoint
    = Scripture Osis
    | UserConf Confirmation
    | UserVal Validity String
    | Remove


type alias RefStuff =
    { refId : RefId
    , ref : Ref
    , refDP : RefDataPoint
    }


type alias Model =
    { percivalData : PercivalData -- get rid of this
    , loadingError : Maybe String
    , keys : Keyboard.Combo.Model Msg
    , isSaving : Bool
    , inEditMode : Bool
    , inHelp : Bool
    , currentDocId : String -- convert to zipper list of docIds w/ doc names
    , blockState : UndoList State -- convert to changes = {past: [], future []}, with only changed block info

    -- move next 6 to type alias EditorModel
    , htmlSource : String
    , editingBlockId : String
    , editorActive : Bool
    , editorTheme : EditorTheme
    , isValidating : Bool
    , htmlValidation : List String

    -- move next 4 to type alias ScriptureListModel
    , selectedRefType : Maybe RefType
    , docRefIds : RefIdArray -- make this a list (RefId, Confirmation)

    --convert the next 2 to zipper List (RefId, Confirmation)
    , currentRefId : RefId
    , listedRefIds : RefIdArray

    -- move next 8 to type alias ActionPanelModel
    , editingOsis : Bool
    , osisField : String
    , badInput : Bool
    , editingContext : Bool
    , contextField : String
    , viewAltRefs : Bool
    , viewScriptureText : Bool
    , scriptureText : String
    }


type alias State =
    { changedBlockId : String
    , blocks : BlockDict
    }


type alias PercivalData =
    { volumeTitle : String
    , parserOpts : Opts
    , docs : DocDict
    , blocks : BlockDict
    }


type alias Osis =
    String


type alias Opts =
    { versification : String
    , language : String
    }


type alias DocDict =
    Dict.Dict String Doc


type alias BlockDict =
    Dict.Dict String Block


type alias RefDict =
    Dict.Dict RefId Ref


type alias Doc =
    { name : String }


type alias Block =
    { html : String
    , refs : RefDict
    }


type alias Ref =
    { text : String
    , data : RefData
    }


type alias RefData =
    { scripture : Osis
    , valid : Bool
    , message : String
    , confidence : Int
    , possible : List Osis
    , confirmed : Bool
    }


type alias RefId =
    String


type alias ScrollList =
    Bool


type alias ScrollDoc =
    Bool


type alias RefIdArray =
    Array.Array ( RefId, Confirmation )


type alias Messages =
    { messages : List ValidatorMessage }


type alias ValidatorMessage =
    { message : String }


type Msg
    = DoNothing
    | NoOp (Result Dom.Error ())
    | LoadData (Result Http.Error PercivalData)
    | ComboMsg Keyboard.Combo.Msg
    | Undo
    | Redo
    | ToggleHelp
    | ListRefsByType (Maybe RefType)
    | ToDoc NavDir
    | ToRef NavDir (Maybe Confirmation)
    | HandleBlockRefClick RefId
    | HandleListRefClick RefId
    | ToggleAltRefs
    | SetScripText (Result Http.Error String)
    | ShowScripture Osis
    | HandlePostResponse (Result Http.Error Block)
    | ChangeRefData RefDataPoint
    | EditOsis
    | UpdateField String
    | ChangeOsis
    | HandleParserResponse (Result Http.Error RefData)
    | EditContext Bool
    | UpdateContextField String
    | AddContextToBlock
    | EditBlock String
    | UpdateSource String
    | ToggleEditorTheme
    | CancelHtml
    | RevertHtml
    | SubmitHtml
    | HandleMessages (Result Http.Error Messages)
    | HandlePostHtml (Result Http.Error Block)
