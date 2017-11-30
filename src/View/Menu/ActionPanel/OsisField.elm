module View.Menu.ActionPanel.OsisField exposing (viewOsisField)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onBlur, onClick, onDoubleClick, onInput)
import Types exposing (..)
import View.Util exposing (onEnter, styles)


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
                [ Attr.class "border-0 b text-uppercase w-100"
                , Attr.value osisField
                , Attr.name "osis"
                , Attr.id "osis-field"
                , onDoubleClick EditOsis
                , Attr.readonly True
                , styles [ color (hex "29363d") ]
                ]
    in
    input attrs []
