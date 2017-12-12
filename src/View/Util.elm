module View.Util exposing (..)

import Css exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (keyCode, on, onWithOptions)
import HtmlParser exposing (..)
import Json.Decode as Json
import Regex exposing (..)
import Types exposing (..)
import Utils exposing (..)


toReadableString : Int -> String
toReadableString num =
    let
        handleMatch match =
            case match.submatches of
                first :: second :: _ ->
                    Maybe.withDefault "" first ++ "," ++ Maybe.withDefault "" second

                _ ->
                    match.match
    in
    num
        |> toString
        |> replace (AtMost 1) (regex "(\\d)(\\d\\d\\d)$") handleMatch
        |> replace (AtMost 1) (regex "(\\d)(\\d\\d\\d,\\d\\d\\d)$") handleMatch


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
            tagName == "a" && isScriptureRef attrs

        hasChildren =
            Basics.not (List.isEmpty children)
    in
    if isRefTag then
        getRefNode (Tuple.second orig) currentRefId
    else if hasChildren then
        getChildNodes orig currentRefId tagName attrs children
    else
        orig


isScriptureRef : Attributes -> Bool
isScriptureRef attrs =
    attrs
        |> List.filter (\( name, value ) -> name == "data-cross-ref" && String.contains "scripture" value)
        |> List.isEmpty
        |> not


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


getKeyValueIfInDict : RefId -> RefDict -> Maybe ( RefId, Ref )
getKeyValueIfInDict refId refs =
    case Dict.get refId refs of
        Nothing ->
            Nothing

        Just ref ->
            Just ( refId, ref )
