module Types exposing
    ( Block
    , BlockDict
    , Confidence(..)
    , Confirmation(..)
    , Dashboard
    , Doc
    , DocDict
    , DocNav(..)
    , DocStats
    , EditorTheme(..)
    , HtmlTrust(..)
    , MarkerData
    , Messages
    , Model
    , Msg(..)
    , NavDir(..)
    , Opts
    , Osis
    , ParsedText
    , PercivalData
    , Ref
    , RefAction(..)
    , RefData
    , RefDataPoint(..)
    , RefDict
    , RefId
    , RefIdArray
    , RefStuff
    , RefType(..)
    , ScrollDoc
    , ScrollList
    , Selection
    , State
    , Stats
    , ValidatorMessage
    , Validity(..)
    )

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


type HtmlTrust
    = Trusted
    | Untrusted


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


type RefAction
    = Multiple ( Validity, Confirmation )
    | Single RefData
    | TextSelection


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
    , selectedRefIds : List RefId
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
    , selection : Selection
    }


type alias State =
    { changedBlockIds : List String
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


type alias Selection =
    { blockId : String
    , selectedText : String
    , anchorOffset : Int
    , focusOffset : Int
    , textContent : String
    }


type alias ParsedText =
    { parsedText : String }


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
    | HandleMultiSelect (List String)
    | ClearSelected
    | HandleListRefClick RefId
    | ToggleAltRefs
    | SetScripText (Result Http.Error String)
    | ShowScripture Osis
    | HideScripture
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
    | SubmitHtml HtmlTrust
    | HandleMessages (Result Http.Error Messages)
    | HandlePostHtml (Result Http.Error Block)
    | TrySelection
    | HandleTextSelection Selection
    | ParseSelection
    | HandleTextParserResponse (Result Http.Error ParsedText)
