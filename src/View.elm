module View exposing (..)

import Css exposing (..)
import Css.Colors exposing (..)
import Dict exposing (..)
import Dom.Scroll exposing (toTop, toY)
import Html exposing (..)
import Html.Attributes as Attr exposing (attribute, class, classList, href, id, src, style, type_)
import Html.Events exposing (onClick, onWithOptions)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import HtmlParser exposing (..)
import HtmlParser.Util exposing (..)
import Json.Decode as Json
import Regex exposing (..)
import Types exposing (..)
import Utils exposing (..)


onClickNoop : msg -> Attribute msg
onClickNoop message =
    let
        options =
            { stopPropagation = True
            , preventDefault = False
            }
    in
    onWithOptions "click" options (Json.succeed message)


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just errorMessage ->
            div [ Attr.class "container" ]
                [ div [ Attr.class "row" ]
                    [ div [ Attr.class "col text-center" ]
                        [ h2 [ Attr.class "pt-5 pb-3" ] [ Html.text errorMessage ]
                        , iframe
                            [ attribute "allowFullScreen" ""
                            , Attr.class "giphy-embed"
                            , attribute "frameBorder" "0"
                            , attribute "height" "96"
                            , Attr.src "https://giphy.com/embed/13dgzBVKmIKCsg"
                            , attribute "width" "480"
                            ]
                            []
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
        [ lazy viewHelp model
        , lazy viewMain model
        , lazy viewMenu model
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
                [ div [ Attr.class "modal-header" ]
                    [ h4 [ Attr.class "modal-title" ]
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
                , div [ Attr.class "modal-body" ] [ viewShortcuts ]
                ]
            ]
        ]


viewShortcuts : Html msg
viewShortcuts =
    h6 [] [ Html.text "Help" ]


viewMain : Model -> Html Msg
viewMain model =
    let
        docBlocks =
            getDocBlocks model

        currentBlockNodes =
            Dict.map (\k v -> viewBlock k v model.currentRefId) docBlocks
                |> Dict.values

        styles =
            Css.asPairs >> Attr.style

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
        , styles [ marginRight (px 400) ]
        ]
        [ lazy2 viewHeader model.isSaving docName
        , div
            [ styles [ marginTop (px 75) ]
            , Attr.class "container-fluid"
            ]
            [ div [ Attr.class "animated fadeIn row" ]
                [ div
                    [ Attr.class "col col-sm-12" ]
                    [ div [ Attr.class "card pb-4" ]
                        currentBlockNodes
                    ]
                ]
            ]
        ]


viewHeader : Bool -> String -> Html Msg
viewHeader isSaving docName =
    let
        styles =
            Css.asPairs >> Attr.style

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


belongsToDoc : String -> String -> Bool
belongsToDoc currentDocId blockKey =
    let
        docBlockRegex =
            String.concat [ "^", currentDocId ]
    in
    contains (regex docBlockRegex) blockKey


getDocBlocks : Model -> BlockDict
getDocBlocks model =
    model.blockState.present
        |> Dict.filter (\k v -> belongsToDoc model.currentDocId k)


viewBlock : String -> Block -> String -> Html Msg
viewBlock blockId block currentRefId =
    let
        parsed =
            HtmlParser.parse block.html

        nodes =
            processNodes parsed block.refs currentRefId
                |> Tuple.first
    in
    div [ Attr.class "blocks", Attr.id blockId ] (toVirtualDom nodes)


processNodes : List Node -> RefDict -> String -> ( List Node, RefDict )
processNodes nodes refs currentRefId =
    let
        processed =
            case List.head nodes of
                Nothing ->
                    ( Text "", refs )

                Just node ->
                    processNode node refs currentRefId

        processedNode =
            Tuple.first processed

        remainingRefs =
            Tuple.second processed
    in
    case List.tail nodes of
        Nothing ->
            ( [ processedNode ], remainingRefs )

        Just nodesTail ->
            let
                processedSiblings =
                    processNodes nodesTail remainingRefs currentRefId

                siblingNodes =
                    Tuple.first processedSiblings

                trueRemaining =
                    Tuple.second processedSiblings
            in
            ( List.concat [ [ processedNode ], siblingNodes ], trueRemaining )


processNode : Node -> RefDict -> String -> ( Node, RefDict )
processNode node refs currentRefId =
    case node of
        Text str ->
            ( node, refs )

        Element tagName attrs children ->
            processElement ( node, refs ) currentRefId tagName attrs children

        Comment str ->
            ( node, refs )


processElement : ( Node, RefDict ) -> String -> String -> Attributes -> List Node -> ( Node, RefDict )
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


getChildNodes : ( Node, RefDict ) -> String -> String -> Attributes -> List Node -> ( Node, RefDict )
getChildNodes orig currentRefId tagName attrs children =
    let
        processed =
            processNodes children (Tuple.second orig) currentRefId

        processedNodes =
            Tuple.first processed

        remainingRefs =
            Tuple.second processed
    in
    ( Element tagName attrs processedNodes, remainingRefs )


getRefNode : RefDict -> String -> ( Node, RefDict )
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
                    else if isFullConf goodRef then
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
                                    , " p-1 ref ref--highlighted"
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


getDocRefs : Model -> List ( String, Ref )
getDocRefs model =
    let
        blockList =
            getDocBlocks model
                |> Dict.values
    in
    List.map (\block -> block.refs |> Dict.toList) blockList
        |> List.concat


filterRefs : Maybe RefType -> List ( String, Ref ) -> List ( String, Ref )
filterRefs mayRefType refTupList =
    case mayRefType of
        Nothing ->
            refTupList

        Just refType ->
            case refType of
                FullConf ->
                    List.filter (\refTup -> isFullConf (Tuple.second refTup)) refTupList

                LowConf ->
                    List.filter (\refTup -> isLowConf (Tuple.second refTup)) refTupList

                Invalid ->
                    List.filter (\refTup -> isInvalid (Tuple.second refTup)) refTupList


viewMenu : Model -> Html Msg
viewMenu model =
    let
        styles =
            Css.asPairs >> Attr.style

        docRefs =
            getDocRefs model

        filteredRefs =
            docRefs |> filterRefs model.listedRefs

        refList =
            Dict.values (Dict.fromList docRefs)
    in
    aside
        [ Attr.class "aside-menu"
        , styles
            [ Css.width (px 400)
            ]
        ]
        [ lazy viewTypeNav refList
        , div
            [ styles [ borderTopColor (rgba 170 170 170 1.0) ]
            , Attr.class "tab-content"
            ]
            [ div
                [ Attr.class "scripture-list"
                , styles
                    [ overflowY scroll
                    , marginBottom (px 400)
                    , Css.height (calc (pct 100) minus (px 430))
                    ]
                ]
                (viewScriptureList model.currentRefId filteredRefs)
            , div
                [ Attr.class "action-panel p-3"
                , styles
                    [ position fixed
                    , bottom (pct 0)
                    , Css.width (pct 100)
                    , Css.height (px 475)
                    , backgroundColor white
                    , borderTop3 (px 1) solid (rgba 170 170 170 1.0)
                    , maxHeight (vh 60)
                    , overflowY scroll
                    ]
                ]
                (viewActionPanel model)
            ]
        ]


viewTypeNav : List Ref -> Html Msg
viewTypeNav refList =
    let
        styles =
            Css.asPairs >> Attr.style

        fullConfCnt =
            getRefCount FullConf refList

        lowConfCnt =
            getRefCount LowConf refList

        invalidCnt =
            getRefCount Invalid refList

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
                    [ onClick (ShowRefs Nothing)
                    , theHeight
                    , Attr.class "callout callout-info my-0 py-0"
                    ]
                    [ Html.small [ Attr.class "text-muted" ]
                        [ Html.text "Total" ]
                    , br [] []
                    , strong [] [ Html.text (toString (List.length refList)) ]
                    ]
                ]
            , div [ Attr.class "col col-3 mx-0 px-0" ]
                [ div
                    [ onClick (ShowRefs (Just FullConf))
                    , theHeight
                    , Attr.class "callout callout-success my-0 py-0"
                    ]
                    [ Html.small [ Attr.class "text-muted" ]
                        [ Html.text "Confirmed" ]
                    , br [] []
                    , strong [] [ Html.text (toString fullConfCnt) ]
                    ]
                ]
            , div [ Attr.class "col col-3 mx-0 px-0" ]
                [ div
                    [ onClick (ShowRefs (Just LowConf))
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
                    [ onClick (ShowRefs (Just Invalid))
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


viewScriptureList : String -> List ( String, Ref ) -> List (Html Msg)
viewScriptureList currentRefId docRefs =
    let
        transHr =
            hr [ Attr.class "transparent mx-3 my-0" ] []

        grayHr =
            hr [ Attr.class "mx-3 my-0" ] []

        beginList =
            [ div [ Attr.class "callout m-0 py-2 text-muted text-center bg-light text-uppercase" ]
                [ Html.small [] [ b [] [ Html.text "Scripture Refs" ] ] ]
            , transHr
            ]

        refList =
            List.map (\refTup -> lazy2 viewScriptureCallout currentRefId refTup) docRefs
                |> List.intersperse grayHr

        endList =
            [ transHr
            , div [ Attr.class "callout m-0 py-2 text-muted text-center bg-light text-uppercase" ]
                [ Html.small [] [ b [] [ Html.text "End of List" ] ] ]
            ]
    in
    List.concat [ beginList, refList, endList ]


viewScriptureCallout : String -> ( String, Ref ) -> Html Msg
viewScriptureCallout currentRefId refTup =
    let
        ref =
            Tuple.second refTup

        refId =
            Tuple.first refTup

        refListId =
            "reflist-" ++ refId

        isCurrent =
            currentRefId == refId

        colorType =
            if isInvalid ref then
                "danger"
            else if isFullConf ref then
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

        styles =
            Css.asPairs >> Attr.style

        osisClasses =
            String.concat [ "col col-7 text-", colorType, " text-right i" ]

        osisText =
            if isInvalid ref then
                ref.data.message
            else
                ref.data.scripture
    in
    div
        [ calloutClassList
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


viewActionPanel : Model -> List (Html Msg)
viewActionPanel model =
    let
        styles =
            Css.asPairs >> Attr.style
    in
    [ div [] [ viewListNav ] ]


viewListNav : Html Msg
viewListNav =
    let
        styles =
            Css.asPairs >> Attr.style
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
            [ span [ Attr.class "nav-link" ]
                [ i
                    [ Attr.class "icon-control-start"
                    , onClick ToPrevDoc
                    ]
                    []
                ]
            ]
        , li [ Attr.class "nav-item" ]
            [ span [ Attr.class "nav-link" ] [ i [ Attr.class "icon-arrow-left" ] [] ] ]
        , li [ Attr.class "nav-item" ]
            [ span [ Attr.class "nav-link" ] [ i [ Attr.class "icon-arrow-up" ] [] ] ]
        , li
            [ styles [ marginLeft auto ]
            , Attr.class "nav-item"
            ]
            [ span [ Attr.class "nav-link" ] [ i [ Attr.class "icon-arrow-down" ] [] ] ]
        , li [ Attr.class "nav-item" ]
            [ span [ Attr.class "nav-link" ] [ i [ Attr.class "icon-arrow-right" ] [] ] ]
        , li [ Attr.class "nav-item" ]
            [ span [ Attr.class "nav-link" ]
                [ i
                    [ Attr.class "icon-control-end"
                    , onClick ToNextDoc
                    ]
                    []
                ]
            ]
        ]
