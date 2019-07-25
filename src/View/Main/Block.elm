module View.Main.Block exposing (viewBlock)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, id)
import Html.Events exposing (onDoubleClick, onMouseUp)
import HtmlParser exposing (..)
import HtmlParser.Util exposing (..)
import Types exposing (..)
import View.Main.Editor exposing (viewEditor)
import View.Util exposing (processNodes)


viewBlock : String -> Block -> Model -> Html Msg
viewBlock blockId block model =
    let
        { currentRefId, selectedRefIds, editorActive } =
            model

        parsed =
            HtmlParser.parse block.html

        nodes =
            processNodes parsed block.refs currentRefId selectedRefIds
                |> Tuple.first
    in
    if editorActive && model.editingBlockId == blockId then
        viewEditor blockId model

    else
        div
            [ Attr.class "blocks"
            , Attr.id blockId
            , onDoubleClick (EditBlock blockId)
            , onMouseUp TrySelection
            ]
            (toVirtualDom nodes)
