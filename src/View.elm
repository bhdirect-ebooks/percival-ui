module View exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (attribute, class, src)
import Types exposing (..)
import View.Help exposing (viewHelp)
import View.Main exposing (viewMain)
import View.Menu exposing (viewMenu)


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just errorMessage ->
            div [ Attr.class "container" ]
                [ div [ Attr.class "row" ]
                    [ div [ Attr.class "col col-12 text-center" ]
                        [ h2 [ Attr.class "pt-5 pb-3" ] [ Html.text "Something went wrong." ]
                        , iframe
                            [ attribute "allowFullScreen" ""
                            , Attr.class "giphy-embed"
                            , attribute "frameBorder" "0"
                            , attribute "height" "96"
                            , Attr.src "https://giphy.com/embed/13dgzBVKmIKCsg"
                            , attribute "width" "480"
                            ]
                            []
                        , p [ Attr.class "b text-center pt-3" ]
                            [ Html.text "It's not global thermonuclear war, but it "
                            , Html.em [] [ Html.text "is" ]
                            , Html.text " this:"
                            ]
                        , p [ Attr.class "text-left" ] [ Html.text errorMessage ]
                        ]
                    ]
                ]


view : Model -> Html Msg
view model =
    let
        modalBackdrop =
            case model.inHelp of
                True ->
                    div [ Attr.class "modal-backdrop fade show" ] []

                False ->
                    span [] []
    in
    div [ Attr.class "app-body" ]
        [ viewHelp model
        , viewMain model
        , viewMenu model
        , modalBackdrop
        ]
