module View.Menu.ActionPanel exposing (viewActionPanel)

import Array exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Types exposing (..)
import View.Menu.ActionPanel.RefActions exposing (viewRefActions)
import View.Menu.ActionPanel.RefContent exposing (viewRefContent)
import View.Menu.ScriptureList.ListNav exposing (viewListNav)


viewActionPanel : RefDict -> Bool -> Model -> List (Html Msg)
viewActionPanel docRefs multiSelect model =
    let
        currentRef =
            Dict.get model.currentRefId docRefs

        listNav =
            viewListNav (Array.length model.docRefIds)

        refActions =
            viewRefActions currentRef multiSelect model

        refContent =
            viewRefContent currentRef model

        fullContents =
            if multiSelect then
                [ refActions ]

            else
                [ listNav, refActions, refContent ]
    in
    [ div [] fullContents ]
