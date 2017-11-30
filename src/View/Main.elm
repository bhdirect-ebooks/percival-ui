module View.Main exposing (..)

import Css exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, id)
import Types exposing (..)
import Utils exposing (..)
import View.Main.Block exposing (viewBlock)
import View.Main.Header exposing (viewHeader)
import View.Util exposing (styles)


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
