module Types exposing (..)

import Array
import Dict
import Dom
import Http
import Keyboard.Combo
import Regex
import UndoList exposing (UndoList)


type EditorTheme
    = Dark
    | Light


type NavDir
    = Prev
    | Next


type DocNav
    = ByDir NavDir
    | ById String


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
    { volumeTitle : String
    , parserOpts : Opts
    , dashboard : Dashboard
    , showDash : Bool
    , docs : DocDict
    , blockState : UndoList State
    , currentDocId : String
    , currentRefId : RefId
    , editingOsis : Bool
    , osisField : String
    , badInput : Bool
    , editingContext : Bool
    , contextField : String
    , htmlSource : String
    , editingBlockId : String
    , editorActive : Bool
    , editorTheme : EditorTheme
    , isValidating : Bool
    , htmlValidation : List String
    , loadingError : Maybe String
    , isSaving : Bool
    , inEditMode : Bool
    , inHelp : Bool
    , docRefIds : RefIdArray
    , selectedRefType : Maybe RefType
    , listedRefIds : RefIdArray
    , keys : Keyboard.Combo.Model Msg
    , viewAltRefs : Bool
    , viewScriptureText : Bool
    , scriptureText : String
    }


type alias State =
    { changedBlockId : String
    , blocks : BlockDict
    }


type alias Dashboard =
    { totals : Stats
    , docStats : List DocStats
    }


type alias DocStats =
    ( String, { name : String, stats : Stats } )


type alias Stats =
    { confirmed : Int
    , lowConfidence : Int
    , invalid : Int
    }


type alias PercivalData =
    { volumeTitle : String
    , parserOpts : Opts
    , docs : DocDict
    , blocks : BlockDict
    , trueDocs : DocDict
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


type alias MarkerData =
    { find : Regex.Regex
    , marker : String
    }


type Msg
    = DoNothing
    | NoOp (Result Dom.Error ())
    | LoadData (Result Http.Error PercivalData)
    | ComboMsg Keyboard.Combo.Msg
    | Undo
    | Redo
    | ToggleDash
    | ToggleHelp
    | ListRefsByType (Maybe RefType)
    | ToDocFromDash String
    | ToDoc DocNav
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
