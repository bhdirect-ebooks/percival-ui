module Main exposing (..)

import Decoders exposing (decodeFileData)
import Http
import Navigation exposing (Location)
import Routing exposing (..)
import Types exposing (..)
import Update exposing (..)
import View exposing (viewOrError)


initialModel : Location -> Route -> Model
initialModel location route =
    { reportData =
        { title = ""
        , structure = []
        , files = []
        , toc =
            { user = ""
            , comp = ""
            }
        , context = ""
        }
    , route = route
    , location = location
    , loadingError = Nothing
    , sidebarMinimized = False
    , sidebarMobileShow = False
    , headDropIsOpen = False
    , listDropIsOpen = False
    , tableDropIsOpen = False
    , quoteDropIsOpen = False
    , asideDropIsOpen = False
    , tocDropIsOpen = False
    , activeLinkId = ""
    , navLinks = []
    }


initialCmd : Cmd Msg
initialCmd =
    decodeFileData
        |> Http.get "http://localhost:3000/data.json"
        |> Http.send LoadData


init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            Routing.parseLocation location
    in
    ( initialModel location currentRoute, initialCmd )


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = viewOrError
        , update = update
        , subscriptions = \_ -> Sub.none
        }
