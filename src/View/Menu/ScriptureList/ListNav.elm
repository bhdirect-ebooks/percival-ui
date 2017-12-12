module View.Menu.ScriptureList.ListNav exposing (viewListNav)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onClick)
import Types exposing (..)
import View.Util exposing (styles)


viewListNav : Int -> Html Msg
viewListNav refCnt =
    let
        fsLg =
            styles [ fontSize (Css.em 1.45) ]

        fsSm =
            styles
                [ fontSize (Css.em 0.9)
                , fontWeight bold
                ]
    in
    ul
        [ styles
            [ marginTop (Css.rem -1)
            , marginLeft (Css.rem -1)
            , Css.width (px 400)
            , borderBottomColor (rgba 170 170 170 1.0)
            ]
        , Attr.class "nav nav-tabs"
        ]
        [ li [ Attr.class "nav-item" ]
            [ span
                [ Attr.class "nav-link"
                , onClick (ToDoc (ByDir Prev))
                ]
                [ i [ Attr.class "icon-control-start text-muted", fsSm ] [] ]
            ]
        , li [ Attr.class "nav-item" ]
            [ span
                [ Attr.class "nav-link"
                , onClick (ToRef Prev (Just Unconfirmed))
                ]
                [ i [ Attr.class "fa fa-angle-double-up text-muted", fsLg ] [] ]
            ]
        , li [ Attr.class "nav-item" ]
            [ span
                [ Attr.class "nav-link"
                , onClick (ToRef Prev Nothing)
                ]
                [ i [ Attr.class "fa fa-angle-up text-muted", fsLg ] [] ]
            ]
        , li
            [ styles [ margin2 (pct 0) auto ]
            , Attr.class "nav-item"
            ]
            [ span
                [ Attr.class "nav-link text-uppercase text-muted"
                , onClick (ListRefsByType Nothing)
                ]
                [ Html.small [] [ b [] [ Html.text ("Total: " ++ toString refCnt) ] ] ]
            ]
        , li [ Attr.class "nav-item" ]
            [ span
                [ Attr.class "nav-link"
                , onClick (ToRef Next Nothing)
                ]
                [ i [ Attr.class "fa fa-angle-down text-muted", fsLg ] [] ]
            ]
        , li [ Attr.class "nav-item" ]
            [ span
                [ Attr.class "nav-link"
                , onClick (ToRef Next (Just Unconfirmed))
                ]
                [ i [ Attr.class "fa fa-angle-double-down text-muted", fsLg ] [] ]
            ]
        , li [ Attr.class "nav-item" ]
            [ span
                [ Attr.class "nav-link"
                , onClick (ToDoc (ByDir Next))
                ]
                [ i [ Attr.class "icon-control-end text-muted", fsSm ] [] ]
            ]
        ]
