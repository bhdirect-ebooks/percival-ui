module View.PageContainer exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, type_)
import Types exposing (..)


viewPageContainer : String -> String -> Html Msg -> Html Msg
viewPageContainer bookTitle pageTitle pageContent =
    main_ [ class "main" ]
        [ ol
            [ class "breadcrumb" ]
            [ li [ class "breadcrumb-item" ]
                [ span [] [ text bookTitle ] ]
            , li [ class "breadcrumb-item" ]
                [ span [] [ text pageTitle ] ]
            ]
        , div [ class "container-fluid" ]
            [ div [ class "animated fadeIn" ]
                [ div [ class "row" ]
                    [ div
                        [ class "col col-sm-12" ]
                        [ pageContent ]
                    ]
                ]
            ]
        ]
