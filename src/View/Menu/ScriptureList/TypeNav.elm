module View.Menu.ScriptureList.TypeNav exposing (viewTypeNav)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onClick)
import Types exposing (..)
import Utils exposing (..)
import View.Util exposing (styles)


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
