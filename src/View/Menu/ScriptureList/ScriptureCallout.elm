module View.Menu.ScriptureList.ScriptureCallout exposing (viewScriptureCallout)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, id)
import Html.Events exposing (onClick)
import Types exposing (..)
import Utils exposing (..)
import View.Util exposing (styles)


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
