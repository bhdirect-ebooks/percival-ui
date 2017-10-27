module Types exposing (..)

import Dict
import Http


type alias Model =
    { percivalData : PercivalData
    , currentDocId : String
    , currentRefId : String
    , loadingError : Maybe String
    , isSaving : Bool
    , inEditMode : Bool
    }


type alias PercivalData =
    { volumeTitle : String
    , parserOpts : Opts
    , docs : Dict.Dict String Doc
    , blocks : Dict.Dict String Block
    }


type alias Osis =
    String


type alias Opts =
    { versification : String
    , language : String
    }


type alias Doc =
    { name : String
    , navOrder : Int
    }


type alias Block =
    { html : String
    , refs : Dict.Dict String Ref
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
