module View exposing (..)

import Ace
import Array exposing (..)
import Css exposing (..)
import Css.Colors exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (attribute, class, classList, href, id, src, style, type_)
import Html.Events exposing (keyCode, on, onBlur, onClick, onDoubleClick, onFocus, onInput, onWithOptions)
import HtmlParser exposing (..)
import HtmlParser.Util exposing (..)
import Json.Decode as Json
import Types exposing (..)
import Utils exposing (..)


styles : List Style -> Attribute msg
styles =
    Css.asPairs >> Attr.style


onClickNoop : msg -> Attribute msg
onClickNoop message =
    let
        options =
            { stopPropagation = True
            , preventDefault = False
            }
    in
    onWithOptions "click" options (Json.succeed message)


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)


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


viewMain : Model -> Html Msg
viewMain model =
    let
        docBlocks =
            getDocBlocks model

        docName =
            Dict.get model.currentDocId model.percivalData.docs
                |> (\doc ->
                        case doc of
                            Nothing ->
                                ""

                            Just doc ->
                                doc.name
                   )
    in
    main_
        [ Attr.class "main"
        , styles
            [ marginRight (px 400)
            , height (vh 100)
            ]
        ]
        [ viewHeader model.isSaving docName
        , div
            [ styles [ marginTop (px 75) ]
            , Attr.class "animated fadeIn container-fluid"
            ]
            [ div [ Attr.class "row" ]
                [ div
                    [ Attr.class "col col-sm-12" ]
                    [ div
                        [ styles
                            [ position relative
                            , maxHeight (calc (calc (vh 100) minus (px 75)) minus (Css.rem 1.5))
                            , overflowY scroll
                            ]
                        , Attr.class "card pb-4"
                        , Attr.id "title-doc"
                        ]
                        (docBlocks
                            |> Dict.toList
                            |> List.map (\( k, v ) -> viewBlock k v model)
                        )
                    ]
                ]
            ]
        ]


viewHeader : Bool -> String -> Html Msg
viewHeader isSaving docName =
    let
        whiteBg =
            styles [ backgroundColor white ]

        savingSpan =
            case isSaving of
                False ->
                    span [ Attr.class "btn" ] []

                True ->
                    span [ Attr.class "text-danger btn" ] [ Html.text "Saving..." ]
    in
    header
        [ styles
            [ position fixed
            , zIndex (int 99)
            , Css.width (calc (pct 100) minus (px 400))
            ]
        ]
        [ ol
            [ styles [ borderBottomColor (rgba 170 170 170 1.0) ]
            , Attr.class "breadcrumb"
            ]
            [ li [ Attr.class "breadcrumb-item" ]
                [ span [] [ Html.text docName ] ]
            , li [ Attr.class "breadcrumb-menu" ]
                [ div [ Attr.class "btn-group" ]
                    [ savingSpan
                    , button
                        [ onClick Undo
                        , whiteBg
                        , Attr.class "btn"
                        ]
                        [ i [ Attr.class "icon-action-undo" ] []
                        , Html.text "  Undo"
                        ]
                    , button
                        [ onClick Redo
                        , whiteBg
                        , Attr.class "btn"
                        ]
                        [ i [ Attr.class "icon-action-redo" ] []
                        , Html.text "  Redo"
                        ]
                    , button
                        [ onClick ToggleHelp
                        , whiteBg
                        , Attr.class "btn"
                        ]
                        [ i [ Attr.class "icon-question" ] []
                        , Html.text "  Help"
                        ]
                    ]
                ]
            ]
        ]


viewBlock : String -> Block -> Model -> Html Msg
viewBlock blockId block model =
    let
        { currentRefId, editorActive } =
            model

        parsed =
            HtmlParser.parse block.html

        nodes =
            processNodes parsed block.refs currentRefId
                |> Tuple.first
    in
    if editorActive && model.editingBlockId == blockId then
        viewEditor blockId model
    else
        div
            [ Attr.class "blocks"
            , Attr.id blockId
            , onDoubleClick (EditBlock blockId)
            ]
            (toVirtualDom nodes)


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


processNodes : List Node -> RefDict -> RefId -> ( List Node, RefDict )
processNodes nodes refs currentRefId =
    let
        ( processedNode, remainingRefs ) =
            case List.head nodes of
                Nothing ->
                    ( Text "", refs )

                Just node ->
                    processNode node refs currentRefId
    in
    case List.tail nodes of
        Nothing ->
            ( [ processedNode ], remainingRefs )

        Just nodesTail ->
            let
                ( siblingNodes, trueRemaining ) =
                    processNodes nodesTail remainingRefs currentRefId
            in
            ( List.concat [ [ processedNode ], siblingNodes ], trueRemaining )


processNode : Node -> RefDict -> RefId -> ( Node, RefDict )
processNode node refs currentRefId =
    case node of
        Text str ->
            ( node, refs )

        Element tagName attrs children ->
            processElement ( node, refs ) currentRefId tagName attrs children

        Comment str ->
            ( node, refs )


processElement : ( Node, RefDict ) -> RefId -> String -> Attributes -> List Node -> ( Node, RefDict )
processElement orig currentRefId tagName attrs children =
    let
        attrNames =
            Tuple.first (List.unzip attrs)

        isRefTag =
            tagName == "a" && List.member "data-cross-ref" attrNames

        hasChildren =
            Basics.not (List.isEmpty children)
    in
    if isRefTag then
        getRefNode (Tuple.second orig) currentRefId
    else if hasChildren then
        getChildNodes orig currentRefId tagName attrs children
    else
        orig


getChildNodes : ( Node, RefDict ) -> RefId -> String -> Attributes -> List Node -> ( Node, RefDict )
getChildNodes orig currentRefId tagName attrs children =
    let
        ( processedNodes, remainingRefs ) =
            processNodes children (Tuple.second orig) currentRefId
    in
    ( Element tagName attrs processedNodes, remainingRefs )


getRefNode : RefDict -> RefId -> ( Node, RefDict )
getRefNode refs currentRefId =
    let
        refId =
            getFirstIdOfDict refs

        ref =
            Dict.get refId refs

        remainingRefs =
            Dict.remove refId refs

        isCurrent =
            currentRefId == refId

        colorType =
            case ref of
                Nothing ->
                    ""

                Just goodRef ->
                    if isInvalid goodRef then
                        "danger"
                    else if isConfirmed goodRef then
                        "success"
                    else
                        "warning"

        attrs =
            case ref of
                Nothing ->
                    []

                Just goodRef ->
                    let
                        classes =
                            if isCurrent then
                                String.concat
                                    [ "btn-outline-"
                                    , colorType
                                    , " p-1 ref ref--selected"
                                    ]
                            else
                                String.concat
                                    [ "btn-outline-"
                                    , colorType
                                    , " p-1 b ref"
                                    ]
                    in
                    [ ( "class", classes )
                    , ( "id", refId )
                    , ( "onclick", "reportRefId('" ++ refId ++ "')" )
                    ]

        children =
            case ref of
                Nothing ->
                    []

                Just goodRef ->
                    [ Text goodRef.text ]

        node =
            Element "span" attrs children
    in
    ( node, remainingRefs )


viewMenu : Model -> Html Msg
viewMenu model =
    let
        docRefs =
            getDocRefs model
                |> Dict.fromList

        refList =
            Dict.values docRefs
    in
    aside
        [ Attr.class "aside-menu"
        , styles
            [ Css.width (px 400)
            ]
        ]
        [ viewTypeNav refList
        , div
            [ styles
                [ borderTopColor (rgba 170 170 170 1.0)
                , height (vh 100)
                ]
            , Attr.class "tab-content"
            ]
            [ div
                [ Attr.id "scripture-list"
                , styles
                    [ overflowY scroll
                    , Css.height (calc (pct 100) minus (px 561))
                    , position relative
                    ]
                ]
                (viewScriptureList docRefs model)
            , div
                [ Attr.class "action-panel p-3"
                , styles
                    [ position fixed
                    , bottom (pct 0)
                    , Css.width (px 400)
                    , Css.height (px 515)
                    , minHeight (px 475)
                    , backgroundColor white
                    , borderTop3 (px 1) solid (rgba 170 170 170 1.0)
                    , maxHeight (calc (vh 100) minus (px 46))
                    ]
                ]
                (viewActionPanel docRefs model)
            ]
        ]


viewTypeNav : List Ref -> Html Msg
viewTypeNav refList =
    let
        confCnt =
            getRefCount (Confirm Confirmed) refList

        lowConfCnt =
            getRefCount (RefConf NotFull) refList

        invalidCnt =
            getRefCount (RefVal Invalid) refList

        unconfCnt =
            getRefCount (Confirm Unconfirmed) refList

        theHeight =
            styles [ Css.height (px 46) ]
    in
    div [ Attr.class "container-fluid m-0" ]
        [ div
            [ theHeight
            , Attr.class "row"
            ]
            [ div [ Attr.class "col col-3 mx-0 px-0" ]
                [ div
                    [ onClick (ListRefsByType (Just (Confirm Confirmed)))
                    , theHeight
                    , Attr.class "callout callout-success my-0 py-0"
                    ]
                    [ Html.small [ Attr.class "text-muted" ]
                        [ Html.text "Confirmed" ]
                    , br [] []
                    , strong [] [ Html.text (toString confCnt) ]
                    ]
                ]
            , div [ Attr.class "col col-3 mx-0 px-0" ]
                [ div
                    [ onClick (ListRefsByType (Just (Confirm Unconfirmed)))
                    , theHeight
                    , Attr.class "callout my-0 py-0"
                    ]
                    [ Html.small [ Attr.class "text-muted" ]
                        [ Html.text "Unconf" ]
                    , br [] []
                    , strong [] [ Html.text (toString unconfCnt) ]
                    ]
                ]
            , div [ Attr.class "col col-3 mx-0 px-0" ]
                [ div
                    [ onClick (ListRefsByType (Just (RefConf NotFull)))
                    , theHeight
                    , Attr.class "callout callout-warning my-0 py-0"
                    ]
                    [ Html.small [ Attr.class "text-muted" ]
                        [ Html.text "Low Conf" ]
                    , br [] []
                    , strong [] [ Html.text (toString lowConfCnt) ]
                    ]
                ]
            , div [ Attr.class "col col-3 mx-0 px-0" ]
                [ div
                    [ onClick (ListRefsByType (Just (RefVal Invalid)))
                    , theHeight
                    , Attr.class "callout callout-danger my-0 py-0"
                    ]
                    [ Html.small [ Attr.class "text-muted" ]
                        [ Html.text "Not Valid" ]
                    , br [] []
                    , strong [] [ Html.text (toString invalidCnt) ]
                    ]
                ]
            ]
        ]


getKeyValueIfInDict : RefId -> RefDict -> Maybe ( RefId, Ref )
getKeyValueIfInDict refId refs =
    case Dict.get refId refs of
        Nothing ->
            Nothing

        Just ref ->
            Just ( refId, ref )


viewScriptureList : RefDict -> Model -> List (Html Msg)
viewScriptureList docRefs model =
    let
        transHr =
            hr [ Attr.class "transparent mx-3 my-0" ] []

        grayHr =
            hr [ Attr.class "mx-3 my-0" ] []

        beginList =
            [ div [ Attr.class "callout m-0 py-2 text-muted text-center bg-light text-uppercase" ]
                [ Html.small [] [ b [] [ Html.text "Scripture Ref List" ] ] ]
            , transHr
            ]

        refList =
            model.listedRefIds
                |> Array.toList
                |> List.filterMap (\( k, v ) -> getKeyValueIfInDict k docRefs)
                |> List.map (\refTup -> viewScriptureCallout model.currentRefId refTup)
                |> List.intersperse grayHr

        endList =
            [ transHr
            , div [ Attr.class "callout m-0 py-2 text-muted text-center bg-light text-uppercase" ]
                [ Html.small [] [ b [] [ Html.text "End of List" ] ] ]
            ]
    in
    List.concat [ beginList, refList, endList ]


viewScriptureCallout : RefId -> ( RefId, Ref ) -> Html Msg
viewScriptureCallout currentRefId ( refId, ref ) =
    let
        refListId =
            getRefListId refId

        isCurrent =
            currentRefId == refId

        colorType =
            if isInvalid ref then
                "danger"
            else if isConfirmed ref then
                "success"
            else
                "warning"

        calloutClassList =
            classList
                [ ( "callout", True )
                , ( String.concat [ "callout-", colorType ], True )
                , ( "m-0", True )
                , ( "pl-4", True )
                , ( "py-3", True )
                , ( "alert", isCurrent )
                , ( String.concat [ "alert-", colorType ], isCurrent )
                ]

        osisClasses =
            String.concat [ "col col-7 text-", colorType, " text-right i" ]

        osisText =
            if isInvalid ref then
                ref.data.message
            else
                ref.data.scripture
    in
    div
        [ onClick (HandleListRefClick refId)
        , calloutClassList
        , Attr.id refListId
        ]
        [ div [ Attr.class "container-fluid" ]
            [ div [ Attr.class "row" ]
                [ div
                    [ styles [ color (hex "3a3a3a") ]
                    , Attr.class "col col-5 pl-0 b"
                    ]
                    [ Html.text ref.text ]
                , div [ Attr.class osisClasses ]
                    [ Html.text osisText ]
                ]
            ]
        ]


viewActionPanel : RefDict -> Model -> List (Html Msg)
viewActionPanel docRefs model =
    let
        currentRef =
            Dict.get model.currentRefId docRefs
    in
    [ div []
        [ viewListNav (Array.length model.docRefIds)
        , viewRefActions currentRef model
        , viewRefContent currentRef model
        ]
    ]


viewListNav : Int -> Html Msg
viewListNav refCnt =
    let
        fsLg =
            styles [ fontSize (Css.em 1.45) ]

        fsSm =
            styles
                [ fontSize (Css.em 0.9)
                , fontWeight bold
                ]
    in
    ul
        [ styles
            [ marginTop (Css.rem -1)
            , marginLeft (Css.rem -1)
            , Css.width (px 400)
            , borderBottomColor (rgba 170 170 170 1.0)
            ]
        , Attr.class "nav nav-tabs"
        ]
        [ li [ Attr.class "nav-item" ]
            [ span
                [ Attr.class "nav-link"
                , onClick (ToDoc Prev)
                ]
                [ i [ Attr.class "icon-control-start text-muted", fsSm ] [] ]
            ]
        , li [ Attr.class "nav-item" ]
            [ span
                [ Attr.class "nav-link"
                , onClick (ToRef Prev (Just Unconfirmed))
                ]
                [ i [ Attr.class "fa fa-angle-double-up text-muted", fsLg ] [] ]
            ]
        , li [ Attr.class "nav-item" ]
            [ span
                [ Attr.class "nav-link"
                , onClick (ToRef Prev Nothing)
                ]
                [ i [ Attr.class "fa fa-angle-up text-muted", fsLg ] [] ]
            ]
        , li
            [ styles [ margin2 (pct 0) auto ]
            , Attr.class "nav-item"
            ]
            [ span
                [ Attr.class "nav-link text-uppercase text-muted"
                , onClick (ListRefsByType Nothing)
                ]
                [ Html.small [] [ b [] [ Html.text ("Total: " ++ toString refCnt) ] ] ]
            ]
        , li [ Attr.class "nav-item" ]
            [ span
                [ Attr.class "nav-link"
                , onClick (ToRef Next Nothing)
                ]
                [ i [ Attr.class "fa fa-angle-down text-muted", fsLg ] [] ]
            ]
        , li [ Attr.class "nav-item" ]
            [ span
                [ Attr.class "nav-link"
                , onClick (ToRef Next (Just Unconfirmed))
                ]
                [ i [ Attr.class "fa fa-angle-double-down text-muted", fsLg ] [] ]
            ]
        , li [ Attr.class "nav-item" ]
            [ span
                [ Attr.class "nav-link"
                , onClick (ToDoc Next)
                ]
                [ i [ Attr.class "icon-control-end text-muted", fsSm ] [] ]
            ]
        ]


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


viewOsisField : Model -> Html Msg
viewOsisField { editingOsis, osisField, badInput } =
    let
        editClasses =
            if badInput then
                Attr.class "w-100 no-no shake-effect"
            else if osisField == "" then
                Attr.class "w-100 no-no"
            else
                Attr.class "w-100"

        attrs =
            if editingOsis then
                [ editClasses
                , Attr.value osisField
                , Attr.name "osis"
                , Attr.id "osis-field"
                , onInput UpdateField
                , onBlur ChangeOsis
                , onEnter ChangeOsis
                , styles [ color (hex "29363d") ]
                ]
            else
                [ Attr.class "border-0 b text-uppercase"
                , Attr.value osisField
                , Attr.name "osis"
                , Attr.id "osis-field"
                , onDoubleClick EditOsis
                , Attr.readonly True
                , styles [ color (hex "29363d") ]
                ]
    in
    input attrs []


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


viewRefContent : Maybe Ref -> Model -> Html Msg
viewRefContent ref model =
    case ref of
        Nothing ->
            div [] []

        Just ref ->
            let
                invalid =
                    isInvalid ref
            in
            if invalid then
                div []
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
            else if model.viewScriptureText then
                div
                    [ Attr.class "container ml-0"
                    , styles [ height (pct 100) ]
                    ]
                    [ div [ Attr.class "row mt-4 mb-3" ]
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
                            )
                        ]
                    ]
            else
                button
                    [ type_ "button"
                    , Attr.class "btn btn-outline-secondary mt-4"
                    , onClick (ShowScripture ref.data.scripture)
                    ]
                    [ Html.text "View Passage" ]
