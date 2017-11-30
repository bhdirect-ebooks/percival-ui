module View.Menu.ActionPanel.ActionButtons exposing (viewActionButtons)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (attribute, class, classList, href, id, src, type_)
import Html.Events exposing (keyCode, onClick)
import Types exposing (..)
import View.Util exposing (styles)


viewActionButtons : Model -> RefData -> Html Msg
viewActionButtons { viewAltRefs, inEditMode } data =
    let
        confAttr =
            if data.confirmed || not data.valid || inEditMode then
                Attr.disabled True
            else
                onClick (ChangeRefData (UserConf Confirmed))

        altButton =
            if data.valid && not (List.isEmpty data.possible) then
                if List.length data.possible == 1 then
                    viewSingleAltButton inEditMode data.possible
                else
                    viewDropdownButton inEditMode viewAltRefs data.possible
            else
                span [] []

        rmvAttr =
            if inEditMode then
                Attr.disabled True
            else
                onClick (ChangeRefData Remove)
    in
    div [ Attr.class "row b mt-4" ]
        [ div [ Attr.class "col col-12" ]
            [ div
                [ attribute "aria-label" "Actions"
                , Attr.class "btn-group w-100"
                , attribute "role" "group"
                ]
                [ button
                    [ Attr.class "btn btn-success"
                    , type_ "button"
                    , confAttr
                    ]
                    [ Html.text "Confirm" ]
                , altButton
                , button
                    [ Attr.class "btn btn-outline-danger mr-1"
                    , type_ "button"
                    , styles [ marginLeft auto ]
                    , rmvAttr
                    ]
                    [ Html.text "Remove" ]
                ]
            ]
        ]


viewSingleAltButton : Bool -> List Osis -> Html Msg
viewSingleAltButton inEditMode possible =
    let
        text =
            List.head possible
                |> (\osis ->
                        case osis of
                            Nothing ->
                                ""

                            Just osis ->
                                osis
                   )

        altAttr =
            if inEditMode || text == "" then
                Attr.disabled True
            else
                onClick (ChangeRefData (Scripture text))
    in
    button
        [ Attr.class "btn btn-outline-primary ml-2"
        , type_ "button"
        , styles
            [ minWidth (px 80)
            , maxWidth (px 196)
            ]
        , altAttr
        ]
        [ Html.text text ]


viewDropdownButton : Bool -> Bool -> List Osis -> Html Msg
viewDropdownButton inEditMode showAlt alternates =
    let
        ariaExpanded =
            if showAlt then
                "true"
            else
                "false"

        dropAttr =
            if inEditMode then
                Attr.disabled True
            else
                onClick ToggleAltRefs
    in
    div
        [ Attr.class "btn-group ml-2"
        , attribute "role" "group"
        , dropAttr
        , styles
            [ minWidth (px 80)
            , maxWidth (px 196)
            , border (px 2)
            ]
        ]
        [ button
            [ attribute "aria-expanded" ariaExpanded
            , attribute "aria-haspopup" "true"
            , attribute "data-toggle" "dropdown"
            , Attr.class "btn btn-outline-primary dropdown-toggle"
            , Attr.id "alt-group-drop"
            , type_ "button"
            ]
            [ Html.text "Choose" ]
        , div
            [ attribute "aria-labelledby" "alt-group-drop"
            , classList
                [ ( "dropdown-menu", True )
                , ( "show", showAlt )
                ]
            , styles
                [ maxHeight (px 275)
                , overflowY scroll
                ]
            ]
            (List.map viewAlternate alternates)
        ]


viewAlternate : Osis -> Html Msg
viewAlternate osis =
    span
        [ Attr.class "dropdown-item"
        , onClick (ChangeRefData (Scripture osis))
        ]
        [ Html.text osis ]
