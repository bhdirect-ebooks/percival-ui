module View.Menu.ActionPanel.RefActions exposing (viewRefActions)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (attribute, class)
import Types exposing (..)
import Utils exposing (..)
import View.Menu.ActionPanel.ActionButtons exposing (viewActionButtons)
import View.Menu.ActionPanel.OsisField exposing (viewOsisField)
import View.Util exposing (styles)


viewRefActions : Maybe Ref -> Model -> Html Msg
viewRefActions currentRef model =
    case currentRef of
        Nothing ->
            div [] []

        Just ref ->
            let
                invalid =
                    isInvalid ref

                colorType =
                    if invalid then
                        "danger"

                    else if isConfirmed ref then
                        "success"

                    else
                        "warning"

                osisOrMessage =
                    if invalid then
                        ref.data.message

                    else
                        ref.data.scripture

                confidence =
                    ref.data.confidence * 10
            in
            div [ Attr.class "container-fluid m-0 p-0" ]
                [ div [ Attr.class "row mt-3 mb-1 pt-2 pb-0" ]
                    [ div [ Attr.class "col col-12" ]
                        [ viewOsisField model
                        , div [ Attr.class "progress progress-xs my-2" ]
                            [ div
                                [ attribute "aria-valuemax" "100"
                                , attribute "aria-valuemin" "0"
                                , attribute "aria-valuenow" (toString confidence)
                                , Attr.class ("progress-bar bg-" ++ colorType)
                                , attribute "role" "progressbar"
                                , styles [ width (pct (toFloat confidence)) ]
                                ]
                                []
                            ]
                        ]
                    ]
                , div
                    [ Attr.class "row text-muted"
                    , styles
                        [ fontSize (Css.rem 1.1)
                        , overflow hidden
                        ]
                    ]
                    [ div [ Attr.class "col col-2 pr-0" ]
                        [ Html.small [] [ Html.text "Text:" ] ]
                    , div
                        [ Attr.class "col col-10 pl-0 d-inline-block text-truncate" ]
                        [ Html.small [] [ Html.text ref.text ] ]
                    ]
                , viewActionButtons model ref.data
                ]
