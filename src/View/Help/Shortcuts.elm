module View.Help.Shortcuts exposing (viewShortcuts)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (class)
import View.Util exposing (styles)


viewShortcuts : Html msg
viewShortcuts =
    div [ Attr.class "container" ]
        [ div [ Attr.class "row mt-1" ]
            [ div [ Attr.class "col col-12" ]
                [ h6 [] [ Html.text "Help" ] ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col col-8" ]
                [ Html.small [] [ Html.text "Open/close help screen" ] ]
            , div [ Attr.class "col col-4 text-right" ]
                [ Html.text "h" ]
            ]
        , div [ Attr.class "row mt-3" ]
            [ div [ Attr.class "col col-12" ]
                [ h6 [] [ Html.text "Navigation" ] ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col col-8" ]
                [ Html.small [] [ Html.text "To next document" ] ]
            , div [ Attr.class "col col-4 text-right" ]
                [ Html.text "w" ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col col-8" ]
                [ Html.small [] [ Html.text "To previous document" ] ]
            , div [ Attr.class "col col-4 text-right" ]
                [ Html.text "q" ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col col-8" ]
                [ Html.small [] [ Html.text "To next ref" ] ]
            , div [ Attr.class "col col-4 text-right" ]
                [ Html.text "↓" ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col col-8" ]
                [ Html.small [] [ Html.text "To previous ref" ] ]
            , div [ Attr.class "col col-4 text-right" ]
                [ Html.text "↑" ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col col-8" ]
                [ Html.small [] [ Html.text "To next unconfirmed ref" ] ]
            , div [ Attr.class "col col-4 text-right" ]
                [ Html.text "→" ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col col-8" ]
                [ Html.small [] [ Html.text "To previous unconfirmed ref" ] ]
            , div [ Attr.class "col col-4 text-right" ]
                [ Html.text "←" ]
            ]
        , div [ Attr.class "row mt-3" ]
            [ div [ Attr.class "col col-12" ]
                [ h6 [] [ Html.text "Ref List Contents" ] ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col col-8" ]
                [ Html.small [] [ Html.text "Show all refs" ] ]
            , div
                [ Attr.class "col col-4 text-right"
                , styles [ fontVariant smallCaps ]
                ]
                [ Html.small [] [ Html.text "esc" ] ]
            ]
        , div [ Attr.class "row mt-3" ]
            [ div [ Attr.class "col col-12" ]
                [ h6 [] [ Html.text "Selected Ref Actions" ] ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col col-8" ]
                [ Html.small [] [ Html.text "Remove ref" ] ]
            , div
                [ Attr.class "col col-4 text-right"
                , styles [ fontVariant smallCaps ]
                ]
                [ Html.small [] [ Html.text "delete/backspace" ] ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col col-8" ]
                [ Html.small [] [ Html.text "Confirm ref" ] ]
            , div
                [ Attr.class "col col-4 text-right" ]
                [ Html.text "c" ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col col-8" ]
                [ Html.small [] [ Html.text "Undo" ] ]
            , div
                [ Attr.class "col col-4 text-right" ]
                [ Html.text "u" ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col col-8" ]
                [ Html.small [] [ Html.text "Redo" ] ]
            , div
                [ Attr.class "col col-4 text-right" ]
                [ Html.text "i" ]
            ]
        ]
