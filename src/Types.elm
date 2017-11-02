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
    | UserConf Confirmation


type alias Model =
    { percivalData : PercivalData
    , blockState : UndoList BlockDict
    , currentDocId : String
    , currentRefId : RefId
    , loadingError : Maybe String
    , isSaving : Bool
    , inEditMode : Bool
    , inHelp : Bool
    , docRefIds : Array.Array ( RefId, Confirmation )
    , selectedRefType : Maybe RefType
    , listedRefIds : Array.Array ( RefId, Confirmation )
    , keys : Keyboard.Combo.Model Msg
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
    , possibile : List Osis
    , confirmed : Bool
    }


type alias RefId =
    String


type alias ScrollList =
    Bool


type alias ScrollDoc =
    Bool


type alias RefSelectionData =
    ( ScrollList, ScrollDoc )


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
