module Types exposing (..)

import Dict
import Http
import Keyboard.Combo
import UndoList exposing (UndoList)


type NavDir
    = Prev
    | Next


type RefType
    = FullConf
    | LowConf
    | Invalid


type alias Model =
    { percivalData : PercivalData
    , blockState : UndoList BlockDict
    , currentDocId : String
    , currentRefId : String
    , loadingError : Maybe String
    , isSaving : Bool
    , inEditMode : Bool
    , inHelp : Bool
    , listedRefs : Maybe RefType
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
    Dict.Dict String Ref


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


type Msg
    = DoNothing
    | LoadData (Result Http.Error PercivalData)
    | ComboMsg Keyboard.Combo.Msg
    | Undo
    | Redo
    | ToggleHelp
    | ToNextDoc
    | ToPrevDoc
    | ShowRefs (Maybe RefType)
