module View.Menu.ActionPanel exposing (viewActionPanel)

import Array exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Types exposing (..)
import View.Menu.ActionPanel.RefActions exposing (viewRefActions)
import View.Menu.ActionPanel.RefContent exposing (viewRefContent)
import View.Menu.ScriptureList.ListNav exposing (viewListNav)


viewActionPanel : RefDict -> Model -> List (Html Msg)
viewActionPanel docRefs model =
    let
        currentRef =
            Dict.get model.currentRefId docRefs
    in
    [ div []
        [ viewListNav (Array.length model.docRefIds)
        , viewRefActions currentRef model
        , viewRefContent currentRef model
        ]
    ]
