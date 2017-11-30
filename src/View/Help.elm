module View.Help exposing (viewHelp)

import Html exposing (..)
import Html.Attributes as Attr exposing (attribute, class, classList, href, type_)
import Html.Events exposing (onClick)
import Types exposing (..)
import View.Help.Shortcuts exposing (viewShortcuts)
import View.Util exposing (onClickNoop, styles)


viewHelp : Model -> Html Msg
viewHelp model =
    div
        [ onClick ToggleHelp
        , classList
            [ ( "modal", True )
            , ( "fade", True )
            , ( "show", model.inHelp )
            , ( "d-none", not model.inHelp )
            , ( "d-block", model.inHelp )
            ]
        , attribute "tabindex" "-1"
        ]
        [ div
            [ Attr.class "modal-dialog" ]
            [ div
                [ onClickNoop DoNothing
                , Attr.class "modal-content"
                ]
                [ div [ Attr.class "modal-header pl-3" ]
                    [ h5 [ Attr.class "modal-title" ]
                        [ Html.text "Keyboard shortcuts" ]
                    , button
                        [ onClick ToggleHelp
                        , attribute "aria-label" "Close"
                        , Attr.class "close"
                        , type_ "button"
                        ]
                        [ span
                            [ attribute "aria-hidden" "true" ]
                            [ Html.text "×" ]
                        ]
                    ]
                , div [ Attr.class "modal-body" ]
                    [ viewShortcuts
                    , hr [] []
                    , div [ Attr.class "container mt-1 mb-1" ]
                        [ div [ Attr.class "row" ]
                            [ div [ Attr.class "col col-12 pl-1" ]
                                [ h5 []
                                    [ Html.text "Additional Help" ]
                                ]
                            ]
                        , div [ Attr.class "row mt-2" ]
                            [ div [ Attr.class "col col-12" ]
                                [ h6 [] [ Html.text "Videos - Using Percival" ] ]
                            ]
                        , div [ Attr.class "row" ]
                            [ ol []
                                [ li []
                                    [ a
                                        [ href "https://www.youtube.com/watch?v=r5ognIEjlR0&list=PLifYXRAIGRblYIXC9-SiQOXCnGBb2h71l&index=1"
                                        , Attr.target "_blank"
                                        ]
                                        [ Html.text "Starting and Navigating" ]
                                    ]
                                , li []
                                    [ a
                                        [ href "https://www.youtube.com/watch?v=3QyBhfOZWJ0&list=PLifYXRAIGRblYIXC9-SiQOXCnGBb2h71l&index=2"
                                        , Attr.target "_blank"
                                        ]
                                        [ Html.text "Working with Bible Refs" ]
                                    ]
                                , li []
                                    [ a
                                        [ href "https://www.youtube.com/watch?v=S-6rGmHCcC0&list=PLifYXRAIGRblYIXC9-SiQOXCnGBb2h71l&index=3"
                                        , Attr.target "_blank"
                                        ]
                                        [ Html.text "Continuing, Finishing, and Other Matters" ]
                                    ]
                                , li []
                                    [ a
                                        [ href "https://www.youtube.com/watch?v=xER9_5H0YWQ&list=PLifYXRAIGRblYIXC9-SiQOXCnGBb2h71l&index=4"
                                        , Attr.target "_blank"
                                        ]
                                        [ Html.text "Q&A, Part 1" ]
                                    ]
                                , li []
                                    [ a
                                        [ href "https://www.youtube.com/watch?v=eYVSmMsvGzo&list=PLifYXRAIGRblYIXC9-SiQOXCnGBb2h71l&index=5"
                                        , Attr.target "_blank"
                                        ]
                                        [ Html.text "Q&A, Part 2" ]
                                    ]
                                ]
                            ]
                        , div [ Attr.class "row" ]
                            [ div [ Attr.class "col col-12" ]
                                [ h6 [] [ Html.text "Reference" ] ]
                            ]
                        , div [ Attr.class "row" ]
                            [ ul []
                                [ li []
                                    [ a
                                        [ href "https://github.com/bhdirect-ebooks/percival#percival"
                                        , Attr.target "_blank"
                                        ]
                                        [ Html.text "Percival ReadMe" ]
                                    ]
                                , li []
                                    [ a
                                        [ href "https://style.bhdirect-ebooks.org/code/data_types.html#Scripture-References"
                                        , Attr.target "_blank"
                                        ]
                                        [ Html.text "Scripture Reference Markup" ]
                                    ]
                                , li []
                                    [ a
                                        [ href "https://docs.google.com/spreadsheets/d/1tgzQru2dVaDU-zhaSfym1UuaPh3_Aktq91iDz9L9JtY"
                                        , Attr.target "_blank"
                                        ]
                                        [ Html.text "OSIS Bible Book Abbreviations" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
