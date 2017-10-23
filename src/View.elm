module View exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, type_)
import Html.Events exposing (onClick)
import Types exposing (..)


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just errorMessage ->
            div [] [ p [] [ text errorMessage ] ]


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ viewMenu model
        , viewBody model
        ]


viewHeader : Html Msg
viewHeader =
    header [ class "app-header navbar" ]
        [ span [ class "navbar-brand" ] []
        , button
            [ class "navbar-toggler mobile-sidebar-toggler d-lg-none"
            , type_ "button"
            , onClick ToggleMobileSide
            ]
            [ text "☰" ]
        , ul [ class "nav navbar-nav d-md-down-none mr-auto" ]
            [ li [ class "nav-item" ]
                [ span
                    [ class "nav-link navbar-toggler sidebar-toggler"
                    , onClick ToggleSidebar
                    ]
                    [ text "☰" ]
                ]
            ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    div
        [ classList
            [ ( "app-body", True )
            , ( "sidebar-fixed", True )
            , ( "sidebar-minimized", model.sidebarMinimized )
            , ( "sidebar-mobile-show", model.sidebarMobileShow )
            ]
        ]
        [ viewSidebar model
        , viewPage model
        ]


viewPageContainer : String -> String -> Html Msg -> Html Msg
viewPageContainer bookTitle pageTitle pageContent =
    main_ [ class "main" ]
        [ ol
            [ class "breadcrumb" ]
            [ li [ class "breadcrumb-item" ]
                [ span [] [ text bookTitle ] ]
            , li [ class "breadcrumb-item" ]
                [ span [] [ text pageTitle ] ]
            ]
        , div [ class "container-fluid" ]
            [ div [ class "animated fadeIn" ]
                [ div [ class "row" ]
                    [ div
                        [ class "col col-sm-12" ]
                        [ pageContent ]
                    ]
                ]
            ]
        ]
