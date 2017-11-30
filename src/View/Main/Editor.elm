module View.Main.Editor exposing (viewEditor)

import Ace
import Html exposing (..)
import Html.Attributes as Attr exposing (attribute, class, id, type_)
import Html.Events exposing (onClick)
import Types exposing (..)


viewEditor : String -> Model -> Html Msg
viewEditor blockId { htmlSource, editorTheme, htmlValidation, isValidating } =
    let
        editorThemeStr =
            case editorTheme of
                Light ->
                    "chrome"

                Dark ->
                    "tomorrow_night_eighties"

        isChecked =
            case editorTheme of
                Light ->
                    False

                Dark ->
                    True

        cardClasses =
            if List.isEmpty htmlValidation then
                Attr.class "card d-block m-3 animated fadeIn"
            else
                Attr.class "card card-danger d-block m-3 no-no shake-effect"

        validateSpan =
            if isValidating then
                span [ Attr.class "mr-5 text-secondary" ] [ Html.text "Checking HTML..." ]
            else
                span [] []
    in
    div
        [ Attr.id blockId
        , cardClasses
        ]
        [ div [ Attr.class "card-body" ]
            [ Ace.toHtml
                [ Ace.onSourceChange UpdateSource
                , Ace.value htmlSource
                , Ace.mode "html"
                , Ace.theme editorThemeStr
                , Ace.enableBasicAutocompletion True
                , Ace.enableLiveAutocompletion True
                , Ace.enableSnippets True
                , Ace.showCursor True
                , Ace.showGutter True
                , Ace.showPrintMargin False
                , Ace.highlightActiveLine True
                , Ace.tabSize 2
                , Ace.useSoftTabs True
                , Ace.useWrapMode True
                , Ace.extensions [ "language_tools" ]
                ]
                []
            , div [ Attr.class "text-right mt-3" ]
                [ Html.span [ Attr.class "float-left" ]
                    [ label
                        [ Attr.class "switch switch-text switch-sm switch-pill switch-dark"
                        ]
                        [ input
                            [ Attr.class "switch-input"
                            , type_ "checkbox"
                            , onClick ToggleEditorTheme
                            , Attr.checked isChecked
                            ]
                            []
                        , span
                            [ Attr.class "switch-label"
                            , attribute "data-on" "DRK"
                            , attribute "data-off" "LGT"
                            ]
                            []
                        , span [ Attr.class "switch-handle" ]
                            []
                        ]
                    ]
                , validateSpan
                , Html.button
                    [ Attr.class "btn btn-sm btn-light"
                    , onClick CancelHtml
                    ]
                    [ Html.text "Cancel" ]
                , Html.button
                    [ Attr.class "btn btn-sm btn-secondary ml-3"
                    , onClick RevertHtml
                    ]
                    [ Html.text "Reset" ]
                , Html.button
                    [ Attr.class "btn btn-sm btn-primary ml-3"
                    , onClick SubmitHtml
                    ]
                    [ Html.text "Submit" ]
                , viewValidatorMessages htmlValidation
                ]
            ]
        ]


viewValidatorMessages : List String -> Html Msg
viewValidatorMessages htmlValidation =
    if List.isEmpty htmlValidation then
        span [] []
    else
        ul [ Attr.class "list-group text-left mt-3" ]
            (htmlValidation
                |> List.map
                    (\m ->
                        li
                            [ Attr.class "list-group-item list-group-item-danger" ]
                            [ Html.text m ]
                    )
            )
