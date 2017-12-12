module View.Dashboard exposing (viewDashboard)

import Html exposing (..)
import Html.Attributes as Attr exposing (attribute, class, classList, href, type_)
import Html.Events exposing (onClick)
import Types exposing (..)
import View.Util exposing (onClickNoop, styles, toReadableString)


viewDashboard : Model -> Html Msg
viewDashboard model =
    let
        totals =
            model.dashboard.totals

        totalRefs =
            totals.confirmed + totals.invalid + totals.lowConfidence

        docStatsSorted =
            List.sortBy Tuple.first model.dashboard.docStats
    in
    div
        [ onClick ToggleDash
        , classList
            [ ( "modal", True )
            , ( "fade", True )
            , ( "show", model.showDash )
            , ( "d-none", not model.showDash )
            , ( "d-block", model.showDash )
            ]
        , attribute "tabindex" "-1"
        ]
        [ div
            [ class "modal-dialog modal-primary modal-lg" ]
            [ div
                [ onClickNoop DoNothing
                , class "modal-content"
                ]
                [ div [ class "modal-header pl-3" ]
                    [ h5 [ class "modal-title" ]
                        [ text model.volumeTitle ]
                    , button
                        [ onClick ToggleDash
                        , attribute "aria-label" "Close"
                        , class "close start"
                        , type_ "button"
                        ]
                        [ i [ class "icon-arrow-right" ] [] ]
                    ]
                , div [ class "modal-body" ]
                    [ div
                        [ class "container mt-1 mb-1" ]
                        [ div [ class "row" ]
                            [ div [ class "col-md-12" ]
                                [ div [ class "row" ]
                                    [ div [ class "col-sm-3" ]
                                        [ div [ class "callout callout-info" ]
                                            [ small [ class "text-muted" ]
                                                [ text "Total References" ]
                                            , br []
                                                []
                                            , strong [ class "h4" ]
                                                [ text (toReadableString totalRefs) ]
                                            ]
                                        ]
                                    , div [ class "col-sm-3" ]
                                        [ div [ class "callout callout-success" ]
                                            [ small [ class "text-muted" ]
                                                [ text "Confirmed" ]
                                            , br []
                                                []
                                            , strong [ class "h4" ]
                                                [ text (toReadableString totals.confirmed) ]
                                            ]
                                        ]
                                    , div [ class "col-sm-3" ]
                                        [ div [ class "callout callout-warning" ]
                                            [ small [ class "text-muted" ]
                                                [ text "Low Confidence" ]
                                            , br []
                                                []
                                            , strong [ class "h4" ]
                                                [ text (toReadableString totals.lowConfidence) ]
                                            ]
                                        ]
                                    , div [ class "col-sm-3" ]
                                        [ div [ class "callout callout-danger" ]
                                            [ small [ class "text-muted" ]
                                                [ text "Not Valid" ]
                                            , br []
                                                []
                                            , strong [ class "h4" ]
                                                [ text (toReadableString totals.invalid) ]
                                            ]
                                        ]
                                    ]
                                , br []
                                    []
                                , div [ class "container t-contain m-0 p-0" ]
                                        [ table
                                            [ attribute "aria-busy" "false"
                                            , class "table mb-0 b-table table-hover"
                                            , attribute "outline" ""
                                            ]
                                            [   thead []
                                                [ tr []
                                                    [ th [ class "w-30" ]
                                                        [ text "File" ]
                                                    , th [ class "w-20 text-center" ]
                                                        [ text "Total" ]
                                                    , th [ class "w-50" ]
                                                        [ text "Stats" ]
                                                    ]
                                                ]
                                            , tbody []
                                                (List.map viewDashTableRow docStatsSorted)
                                            ]
                                        ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewDashTableRow : DocStats -> Html Msg
viewDashTableRow (docId, docStat) =
    let
        navId =
            docId ++ "-01"

        total =
            docStat.stats.confirmed
            + docStat.stats.lowConfidence
            + docStat.stats.invalid

        totalStr = total |> toReadableString
    in
    tr [ onClick (ToDocFromDash navId) ]
        [ td []
            [ text docStat.name ]
        , td [ class "text-center" ]
            [ text totalStr ]
        , viewFileStats total docStat.stats
        ]


viewFileStats : Int -> Stats -> Html Msg
viewFileStats total stats =
    let
        ivLines =
            viewDashLines "bg-danger" total stats.invalid

        lcLines =
            viewDashLines "bg-warning" total stats.lowConfidence

        cfLines =
            viewDashLines "bg-success" total stats.confirmed
    in
    td []
        [ div
            [ class "progress progress-xs mt-2" ]
            (List.concat [ cfLines, lcLines, ivLines ])
        ]

viewDashLines : String -> Int -> Int -> List (Html Msg)
viewDashLines bg total cnt =
    let
        pct =
            round (((toFloat cnt) / (toFloat total)) * 100)
            |> toString
    in
    if cnt > 0 then
        [ div
            [ attribute "aria-valuemax" "100"
            , attribute "aria-valuemin" "0"
            , attribute "aria-valuenow" pct
            , class ("progress-bar " ++ bg)
            , attribute "role" "progressbar"
            , attribute "style" ("height: 1rem; line-height: 1rem; width: "++ pct ++ "%")
            ]
            []
        ]
    else
        []