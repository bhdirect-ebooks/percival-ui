module View.Menu.ActionPanel.RefContent exposing (viewRefContent)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (alt, class, href, src, type_)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import HtmlParser exposing (..)
import HtmlParser.Util exposing (..)
import Types exposing (..)
import Utils exposing (..)
import View.Util exposing (onEnter, styles)


viewRefContent : Maybe Ref -> Model -> Html Msg
viewRefContent ref model =
    case ref of
        Nothing ->
            div [] []

        Just ref ->
            let
                invalid =
                    isInvalid ref

                addContextElems =
                    [ div [ Attr.class "row mt-4" ]
                        [ div
                            [ Attr.class "col col-12" ]
                            [ Html.text "Add Context:" ]
                        ]
                    , div [ Attr.class "row mt-1" ]
                        [ div
                            [ Attr.class "col col-12" ]
                            [ input
                                [ Attr.class "w-100"
                                , Attr.value model.contextField
                                , Attr.name "context"
                                , Attr.id "context-field"
                                , onInput UpdateContextField
                                , onFocus (EditContext True)
                                , onBlur (EditContext False)
                                , onEnter AddContextToBlock
                                , styles [ color (hex "29363d") ]
                                ]
                                []
                            ]
                        ]
                    , div [ Attr.class "row mt-1" ]
                        [ div
                            [ Attr.class "col col-12" ]
                            [ Html.small [ Attr.class "text-muted i" ]
                                [ Html.text "Applies parsing context to the entire parent HTML chunk" ]
                            ]
                        ]
                    ]
            in
            if invalid then
                div [] addContextElems

            else if model.viewScriptureText then
                div
                    [ Attr.class "container ml-0"
                    , styles [ height (pct 100) ]
                    ]
                    [ div
                        [ Attr.class "row mt-4 mb-3" ]
                        [ div
                            [ Attr.class "col col-12 border border-left-0 border-right-0 border-bottom-0 pt-4 pb-2 px-0"
                            , styles
                                [ overflowY scroll
                                , position relative
                                , minHeight (px 255)
                                , height (px 255)
                                ]
                            ]
                            ([ div [ Attr.class "row" ]
                                [ div [ Attr.class "col col-3 pr-0 pl-4 pt-1" ]
                                    [ a
                                        [ href "http://biblia.com/" ]
                                        [ img
                                            [ Attr.alt "Powered by Biblia"
                                            , Attr.src "http://api.biblia.com/v1/PoweredByBiblia_small.png"
                                            ]
                                            []
                                        ]
                                    ]
                                , div [ Attr.class "col col-7 pl-2" ]
                                    [ Html.small [ Attr.class "text-muted" ]
                                        [ Html.text "This site uses "
                                        , a [ href "http://biblia.com/" ]
                                            [ Html.text "Biblia" ]
                                        , Html.text " web services from "
                                        , a [ href "http://www.logos.com/" ]
                                            [ Html.text "Logos Bible Software" ]
                                        , Html.text "."
                                        ]
                                    ]
                                ]
                             ]
                                |> List.append (toVirtualDom (parse model.scriptureText))
                                |> List.append
                                    [ button
                                        [ type_ "button"
                                        , styles [ marginTop (Css.em -0.75) ]
                                        , Attr.class "btn btn-outline-secondary float-right"
                                        , onClick HideScripture
                                        ]
                                        [ Html.text "Hide Passage" ]
                                    ]
                            )
                        ]
                    ]

            else
                div []
                    (List.append
                        [ button
                            [ type_ "button"
                            , Attr.class "btn btn-outline-secondary mt-4"
                            , onClick (ShowScripture ref.data.scripture)
                            ]
                            [ Html.text "View Passage" ]
                        ]
                        addContextElems
                    )
