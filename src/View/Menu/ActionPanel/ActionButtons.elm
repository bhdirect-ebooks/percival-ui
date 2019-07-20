module View.Menu.ActionPanel.ActionButtons exposing (viewActionButtons)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (attribute, class, classList, href, id, src, type_)
import Html.Events exposing (keyCode, onClick)
import Types exposing (..)
import View.Util exposing (styles)


viewActionButtons : Model -> RefAction -> Html Msg
viewActionButtons { viewAltRefs, inEditMode } action =
    let
        buttons =
            case action of
                Multiple data ->
                    viewMultipleActions inEditMode data

                Single data ->
                    viewSingleActions viewAltRefs inEditMode data
    in
    div [ Attr.class "row b mt-4" ]
        [ div [ Attr.class "col col-12" ]
            [ div
                [ attribute "aria-label" "Actions"
                , Attr.class "btn-group w-100"
                , attribute "role" "group"
                ]
                buttons
            ]
        ]


viewConfirmButton : Attribute msg -> Html msg
viewConfirmButton confAttr =
    button
        [ Attr.class "btn btn-success"
        , type_ "button"
        , confAttr
        ]
        [ Html.text "Confirm" ]


viewRemoveButton : Attribute msg -> Html msg
viewRemoveButton rmvAttr =
    button
        [ Attr.class "btn btn-outline-danger mr-1 ml-auto"
        , type_ "button"
        , rmvAttr
        ]
        [ Html.text "Remove" ]


getConfirmAttributes : Bool -> Attribute Msg
getConfirmAttributes predicate =
    if predicate then
        Attr.disabled True

    else
        onClick (ChangeRefData (UserConf Confirmed))


getRemoveAttributes : Bool -> Attribute Msg
getRemoveAttributes predicate =
    if predicate then
        Attr.disabled True

    else
        onClick (ChangeRefData Remove)


viewMultipleActions : Bool -> ( Validity, Confirmation ) -> List (Html Msg)
viewMultipleActions inEditMode ( validity, confirmation ) =
    let
        confAttr =
            getConfirmAttributes
                (inEditMode || validity == Invalid || confirmation == Confirmed)

        rmvAttr =
            getRemoveAttributes inEditMode
    in
    [ viewConfirmButton confAttr
    , button
        [ type_ "button"
        , Attr.class "btn btn-outline-secondary ml-auto"
        , onClick ClearSelected
        ]
        [ Html.text "Clear Selection" ]
    , viewRemoveButton rmvAttr
    ]


viewSingleActions : Bool -> Bool -> RefData -> List (Html Msg)
viewSingleActions viewAltRefs inEditMode data =
    let
        confAttr =
            getConfirmAttributes (data.confirmed || not data.valid || inEditMode)

        altButton =
            if data.valid && not (List.isEmpty data.possible) then
                if List.length data.possible == 1 then
                    viewSingleAltButton inEditMode data.possible

                else
                    viewDropdownButton inEditMode viewAltRefs data.possible

            else
                span [] []

        rmvAttr =
            getRemoveAttributes inEditMode
    in
    [ viewConfirmButton confAttr
    , altButton
    , viewRemoveButton rmvAttr
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
