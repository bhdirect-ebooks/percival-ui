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
    | Id RefId


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
    { loadingError : Maybe String
    , keys : Keyboard.Combo.Model Msg
    , isSaving : Bool
    , inEditMode : Bool
    , inHelp : Bool
    , undoList : Changes
    , volumeTitle : String
    , parserOpts : Opts
    , state : UndoList State
    , editor : EditorModel
    , scriptureList : ScriptureListModel
    , actionPanel : ActionPanelModel
    }


type alias PercivalData =
    { volumeTitle : String
    , parserOpts : Opts
    , docs : DocZipper
    }


type alias State =
    { changedBlockId : String
    , docs : DocZipper
    }


type alias DocZipper =
    { prev : List DocId
    , current : DocData
    , next : List DocId
    }


type alias Changes =
    { past : List ( BlockId, Block )
    , future : List ( BlockId, Block )
    }


type alias EditorModel =
    { htmlSource : String
    , editingBlockId : BlockId
    , editorActive : Bool
    , editorTheme : EditorTheme
    , isValidating : Bool
    , htmlValidation : List String
    }


type alias ScriptureListModel =
    { selectedRefType : Maybe RefType
    , docRefIds : List RefConfTuple
    , listedRefs : RefZipper
    }


type alias RefConfTuple =
    ( RefId, Confirmation )


type alias RefZipper =
    { prev : List RefConfTuple
    , current : RefConfTuple
    , next : List RefConfTuple
    }


type alias ActionPanelModel =
    { editingOsis : Bool
    , osisField : String
    , badInput : Bool
    , editingContext : Bool
    , contextField : String
    , viewAltRefs : Bool
    , viewScriptureText : Bool
    , scriptureText : String
    }


type alias Osis =
    String


type alias Opts =
    { versification : String
    , language : String
    }


type alias DocData =
    { docId : DocId
    , name : String
    , blocks : List BlockTuple
    , refs : RefDict
    }


type alias DocDict =
    Dict.Dict String Doc


type alias BlockTuple =
    ( BlockId, BlockHtml )


type alias RefDict =
    Dict.Dict RefId Ref


type alias Doc =
    { name : String }


type alias Block =
    { html : BlockHtml
    , refs : RefDict
    }


type alias BlockHtml =
    String


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


type alias DocId =
    String


type alias BlockId =
    String


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
