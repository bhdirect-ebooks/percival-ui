module Types exposing (..)

import Http


type alias Model =
    { percivalData : PercivalData
    , currentDoc : String
    , currentRef : String
    , loadingError : Maybe String
    , isSaving : Bool
    , inEditMode : Bool
    }


type alias PercivalData =
    { volumeTitle : String
    , parserOpts : Opts
    , docs : List Doc
    , blocks : List Block
    }


type alias Osis =
    String


type alias Opts =
    { versification : String
    , language : String
    }


type alias Doc =
    { id : String
    , name : String
    }


type alias Block =
    { id : String
    , html : String
    , refs : List Ref
    , isSelected : Bool
    }


type alias Ref =
    { id : String
    , text : String
    , data : RefData
    , hasFocus : Bool
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
