module View exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (attribute, class, classList, href, id, src)
import Html.Events exposing (onClick)
import HtmlParser exposing (..)
import HtmlParser.Util exposing (..)
import Regex exposing (..)
import Types exposing (..)


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just errorMessage ->
            div []
                [ h1 [ class "text-center" ] [ text errorMessage ]
                , iframe [ class "mx-auto", attribute "allowfullscreen" "", attribute "frameborder" "0", attribute "height" "315", src "https://www.youtube.com/embed/dQw4w9WgXcQ?rel=0&controls=0&showinfo=0", attribute "width" "560" ] []
                ]


view : Model -> Html Msg
view model =
    let
        title =
            model.percivalData.volumeTitle

        doc =
            Dict.get model.currentDocId model.percivalData.docs
                |> (\doc ->
                        case doc of
                            Nothing ->
                                ""

                            Just doc ->
                                doc.name
                   )
    in
    div [ class "app" ]
        [ div [ class "app-body" ]
            [ viewHeader title doc
            , viewMain model
            , viewMenu model
            ]
        ]


viewHeader : String -> String -> Html Msg
viewHeader title docName =
    header []
        [ ol
            [ class "breadcrumb" ]
            [ li [ class "breadcrumb-item" ]
                [ span [] [ text title ] ]
            , li [ class "breadcrumb-item" ]
                [ span [] [ text docName ] ]
            , li [ class "breadcrumb-menu" ]
                [ div [ class "btn-group" ]
                    [ button [ class "btn" ]
                        [ i [ class "icon-action-undo" ] []
                        , text "  Undo"
                        ]
                    , button [ class "btn" ]
                        [ i [ class "icon-action-redo" ] []
                        , text "  Redo"
                        ]
                    , button [ class "btn" ]
                        [ i [ class "icon-question" ] []
                        , text "  Help"
                        ]
                    ]
                ]
            ]
        ]


viewMain : Model -> Html Msg
viewMain model =
    let
        docBlocks =
            getDocBlocks model

        currentBlockNodes =
            Dict.map (\k v -> viewBlock k v model.currentRefId) docBlocks
                |> Dict.values
    in
    main_ [ class "main" ]
        [ div [ class "container-fluid" ]
            [ div [ class "animated fadeIn" ]
                [ div [ class "row" ]
                    [ div
                        [ class "col col-sm-12" ]
                        [ div [ class "card" ]
                            currentBlockNodes
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


getDocBlocks : Model -> Dict String Block
getDocBlocks model =
    model.percivalData.blocks
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
    div [ class "blocks", id blockId ] (toVirtualDom nodes)


processNodes : List Node -> Dict String Ref -> String -> ( List Node, Dict String Ref )
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


processNode : Node -> Dict String Ref -> String -> ( Node, Dict String Ref )
processNode node refs currentRefId =
    case node of
        Text str ->
            ( node, refs )

        Element tagName attrs children ->
            processElement ( node, refs ) currentRefId tagName attrs children

        Comment str ->
            ( node, refs )


processElement : ( Node, Dict String Ref ) -> String -> String -> Attributes -> List Node -> ( Node, Dict String Ref )
processElement orig currentRefId tagName attrs children =
    let
        attrNames =
            Tuple.first (List.unzip attrs)

        isRefTag =
            tagName == "a" && List.member "data-cross-ref" attrNames

        hasChildren =
            not (List.isEmpty children)
    in
    if isRefTag then
        getRefNode (Tuple.second orig) currentRefId
    else if hasChildren then
        getChildNodes orig currentRefId tagName attrs children
    else
        orig


getChildNodes : ( Node, Dict String Ref ) -> String -> String -> Attributes -> List Node -> ( Node, Dict String Ref )
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


getFirstRefId : Dict String Ref -> String
getFirstRefId refs =
    Dict.keys refs
        |> List.head
        |> (\refId ->
                case refId of
                    Nothing ->
                        ""

                    Just refId ->
                        refId
           )


getRefNode : Dict String Ref -> String -> ( Node, Dict String Ref )
getRefNode refs currentRefId =
    let
        refId =
            getFirstRefId refs

        ref =
            Dict.get refId refs

        remainingRefs =
            Dict.remove refId refs

        attrs =
            case ref of
                Nothing ->
                    []

                Just goodRef ->
                    if refId == currentRefId then
                        [ ( "class", "ref ref--highlighted" ), ( "id", refId ) ]
                    else
                        [ ( "class", "ref" ), ( "id", refId ) ]

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
        theThing =
            ""
    in
    aside [ class "aside-menu-fixed" ]
        [ div [ class "tab-content" ]
            [ div [ class "scripture-list" ]
                [ div [ class "callout callout-warning m-0 py-3" ]
                    [ div []
                        [ text "Meeting with "
                        , strong []
                            [ text "Lucas" ]
                        ]
                    ]
                , hr [ class "mx-3 my-0" ]
                    []
                , div [ class "callout callout-success m-0 py-3" ]
                    [ div []
                        [ text "Skype with "
                        , strong []
                            [ text "Megan" ]
                        ]
                    ]
                , hr [ class "mx-3 my-0" ]
                    []
                , div [ class "callout callout-danger m-0 py-3" ]
                    [ div []
                        [ text "New UI Project - "
                        , strong []
                            [ text "deadline" ]
                        ]
                    ]
                , hr [ class "mx-3 my-0" ]
                    []
                , div [ class "callout callout-success m-0 py-3" ]
                    [ div []
                        [ strong []
                            [ text "#10 Startups.Garden" ]
                        , text "Meetup      "
                        ]
                    ]
                ]
            , div [ class "action-panel p-3" ]
                [ h6 []
                    [ text "Settings" ]
                ]
            ]
        ]
