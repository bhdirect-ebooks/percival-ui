module Types exposing (..)

import Html exposing (..)
import Http
import Navigation exposing (Location)


type IssueClass
    = IssueError
    | IssueWarning
    | IssueLight


type Stage
    = Structure
    | FileIssues
    | ScripIssues
    | HeadOutline
    | ListInspect
    | TableInspect
    | QuoteInspect
    | AsideInspect
    | TocDiff


type alias Model =
    { reportData : FileData
    , route : Route
    , location : Location
    , loadingError : Maybe String
    , sidebarMinimized : Bool
    , sidebarMobileShow : Bool
    , headDropIsOpen : Bool
    , listDropIsOpen : Bool
    , tableDropIsOpen : Bool
    , quoteDropIsOpen : Bool
    , asideDropIsOpen : Bool
    , tocDropIsOpen : Bool
    , activeLinkId : String
    , navLinks : List NavLink
    }


type alias NavLink =
    { href : String
    , id : String
    , isActive : Bool
    , stage : Stage
    , text : String
    }


type alias Name =
    String


type alias FileData =
    { title : String
    , structure : List String
    , files : List File
    , toc : Toc
    , context : String
    }


type alias File =
    { name : Name
    , issues : List Issue
    , scripture : List Issue
    , outline : String
    , lists : String
    , tables : List Section
    , quotes : List Section
    , asides : List Section
    }


type alias Issue =
    { line : String
    , col : String
    , message : String
    , help : String
    , class : IssueClass
    }


type alias Section =
    { line : Int
    , html : String
    }


type alias MainView =
    { stage : Stage
    , title : String
    , content : Html Msg
    }


type alias Toc =
    { user : String
    , comp : String
    }


type Route
    = StructRoute
    | FileRoute
    | ScripRoute
    | TocRoute
    | OutlineRoute Name
    | ListsRoute Name
    | TablesRoute Name
    | QuotesRoute Name
    | AsidesRoute Name
    | NotFoundRoute


type Msg
    = DoNothing
    | LoadData (Result Http.Error FileData)
    | OnLocationChange Location
    | SetActiveLink String
    | ToggleSidebar
    | ToggleMobileSide
    | ToggleHeadDrop
    | ToggleListDrop
    | ToggleTableDrop
    | ToggleQuoteDrop
    | ToggleAsideDrop
