module View.Util exposing
    ( getChildNodes
    , getKeyValueIfInDict
    , getRefNode
    , getRefsByListed
    , isScriptureRef
    , onClickNoop
    , onEnter
    , processElement
    , processNode
    , processNodes
    , refListConfirmation
    , refListValidity
    , styles
    , toReadableString
    )

import Array
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


processNodes : List Node -> RefDict -> RefId -> List RefId -> ( List Node, RefDict )
processNodes nodes refs currentRefId selectedRefIds =
    let
        ( processedNode, remainingRefs ) =
            case List.head nodes of
                Nothing ->
                    ( Text "", refs )

                Just node ->
                    processNode node refs currentRefId selectedRefIds
    in
    case List.tail nodes of
        Nothing ->
            ( [ processedNode ], remainingRefs )

        Just nodesTail ->
            let
                ( siblingNodes, trueRemaining ) =
                    processNodes nodesTail remainingRefs currentRefId selectedRefIds
            in
            ( List.concat [ [ processedNode ], siblingNodes ], trueRemaining )


processNode : Node -> RefDict -> RefId -> List RefId -> ( Node, RefDict )
processNode node refs currentRefId selectedRefIds =
    case node of
        Text str ->
            ( node, refs )

        Element tagName attrs children ->
            processElement ( node, refs ) ( currentRefId, selectedRefIds ) tagName attrs children

        Comment str ->
            ( node, refs )


processElement : ( Node, RefDict ) -> ( RefId, List RefId ) -> String -> Attributes -> List Node -> ( Node, RefDict )
processElement orig refSelection tagName attrs children =
    let
        attrNames =
            Tuple.first (List.unzip attrs)

        isRefTag =
            tagName == "a" && isScriptureRef attrs

        hasChildren =
            Basics.not (List.isEmpty children)

        ( currentRefId, selectedRefIds ) =
            refSelection
    in
    if isRefTag then
        getRefNode (Tuple.second orig) currentRefId selectedRefIds

    else if hasChildren then
        getChildNodes orig refSelection tagName attrs children

    else
        orig


isScriptureRef : Attributes -> Bool
isScriptureRef attrs =
    attrs
        |> List.filter (\( name, value ) -> name == "data-ref")
        |> List.isEmpty
        |> not


getChildNodes : ( Node, RefDict ) -> ( RefId, List RefId ) -> String -> Attributes -> List Node -> ( Node, RefDict )
getChildNodes orig refSelection tagName attrs children =
    let
        ( currentRefId, selectedRefIds ) =
            refSelection

        ( processedNodes, remainingRefs ) =
            processNodes children (Tuple.second orig) currentRefId selectedRefIds
    in
    ( Element tagName attrs processedNodes, remainingRefs )


getRefNode : RefDict -> RefId -> List RefId -> ( Node, RefDict )
getRefNode refs currentRefId selectedRefIds =
    let
        refId =
            getFirstIdOfDict refs

        ref =
            Dict.get refId refs

        remainingRefs =
            Dict.remove refId refs

        isCurrent =
            currentRefId == refId

        isSelected =
            List.member refId selectedRefIds

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
                            if isCurrent || isSelected then
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


getRefsByListed : Model -> List RefData
getRefsByListed model =
    let
        docRefs =
            getDocRefs model
                |> Dict.fromList
    in
    model.listedRefIds
        |> Array.toList
        |> List.filterMap (\( k, v ) -> getKeyValueIfInDict k docRefs)
        |> List.map (\( k, v ) -> v.data)


refListValidity : List RefData -> Validity
refListValidity refDataList =
    let
        allValid =
            refDataList
                |> List.filter (\data -> not data.valid)
                |> List.isEmpty
    in
    case allValid of
        True ->
            Valid

        False ->
            Invalid


refListConfirmation : List RefData -> Confirmation
refListConfirmation refDataList =
    let
        allConfirmed =
            refDataList
                |> List.filter (\data -> not data.confirmed)
                |> List.isEmpty
    in
    case allConfirmed of
        True ->
            Confirmed

        False ->
            Unconfirmed
