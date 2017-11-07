module Types exposing (..)

import Array
import Dict
import Dom
import Http
import Keyboard.Combo
import UndoList exposing (UndoList)


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
    | Remove


type alias RefStuff =
    { refId : RefId
    , ref : Ref
    , refDP : RefDataPoint
    }


type alias Model =
    { percivalData : PercivalData
    , blockState : UndoList State
    , currentDocId : String
    , currentRefId : RefId
    , editingOsis : Bool
    , osisField : String
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
    , isEditing : Bool
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
    | HandleParserResponse String
