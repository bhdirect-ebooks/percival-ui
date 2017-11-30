module View.Menu.ScriptureList exposing (viewScriptureList)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (class)
import Types exposing (..)
import View.Menu.ScriptureList.ScriptureCallout exposing (viewScriptureCallout)
import View.Util exposing (getKeyValueIfInDict)


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
