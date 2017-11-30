module View.Menu exposing (viewMenu)

import Ace
import Array exposing (..)
import Css exposing (..)
import Css.Colors exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (attribute, class, classList, href, id, src, style, type_)
import HtmlParser exposing (..)
import HtmlParser.Util exposing (..)
import Json.Decode as Json
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
    in
    aside
        [ Attr.class "aside-menu"
        , styles
            [ Css.width (px 400)
            ]
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
                    , Css.height (calc (pct 100) minus (px 561))
                    , position relative
                    ]
                ]
                (viewScriptureList docRefs model)
            , div
                [ Attr.class "action-panel p-3"
                , styles
                    [ position fixed
                    , bottom (pct 0)
                    , Css.width (px 400)
                    , Css.height (px 515)
                    , minHeight (px 475)
                    , backgroundColor white
                    , borderTop3 (px 1) solid (rgba 170 170 170 1.0)
                    , maxHeight (calc (vh 100) minus (px 46))
                    ]
                ]
                (viewActionPanel docRefs model)
            ]
        ]
