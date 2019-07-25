module View.Menu exposing (viewMenu)

import Css exposing (..)
import Css.Colors exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, id)
import Types exposing (..)
import Utils exposing (..)
import View.Menu.ActionPanel exposing (viewActionPanel)
import View.Menu.ScriptureList exposing (viewScriptureList)
import View.Menu.ScriptureList.TypeNav exposing (viewTypeNav)
import View.Util exposing (styles)


viewMenu : Model -> Html Msg
viewMenu model =
    let
        docRefs =
            getDocRefs model
                |> Dict.fromList

        refList =
            Dict.values docRefs

        multiSelect =
            List.length model.selectedRefIds > 1

        miniPanelHeight =
            px 150

        actPanelMinHeight =
            if multiSelect then
                miniPanelHeight

            else
                px 475

        actPanelHeight =
            if multiSelect then
                miniPanelHeight

            else
                px 515

        scripListReduction =
            if multiSelect then
                miniPanelHeight

            else
                px 561
    in
    aside
        [ Attr.class "aside-menu"
        , styles [ Css.width (px 400) ]
        ]
        [ viewTypeNav refList
        , div
            [ styles
                [ borderTopColor (rgba 170 170 170 1.0)
                , height (vh 100)
                ]
            , Attr.class "tab-content"
            ]
            [ div
                [ Attr.id "scripture-list"
                , styles
                    [ overflowY scroll
                    , Css.height (calc (pct 100) minus scripListReduction)
                    , position relative
                    ]
                ]
                (viewScriptureList docRefs model)
            , div
                [ Attr.class "action-panel p-3"
                , styles
                    [ bottom (pct 0)
                    , Css.width (px 400)
                    , Css.height actPanelHeight
                    , minHeight actPanelMinHeight
                    , backgroundColor white
                    , borderTop3 (px 1) solid (rgba 170 170 170 1.0)
                    , maxHeight (calc (vh 100) minus (px 46))
                    ]
                ]
                (viewActionPanel docRefs multiSelect model)
            ]
        ]
