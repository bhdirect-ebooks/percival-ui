module View.Page exposing (viewPage)

import Html exposing (..)
import Types exposing (..)
import View.Asides exposing (..)
import View.FileIssues exposing (..)
import View.Lists exposing (..)
import View.Outline exposing (..)
import View.Quotes exposing (..)
import View.ScripIssues exposing (..)
import View.StructIssues exposing (..)
import View.Tables exposing (..)
import View.Toc exposing (..)


viewPage : Model -> Html Msg
viewPage model =
    let
        bookTitle =
            model.reportData.title
    in
    case model.route of
        StructRoute ->
            viewStructure bookTitle model.reportData.structure

        FileRoute ->
            viewFileIssues bookTitle model.reportData.files

        ScripRoute ->
            viewScripIssues bookTitle model.reportData.context model.reportData.files

        TocRoute ->
            viewToc bookTitle model.reportData.toc

        OutlineRoute name ->
            viewOutline model name

        ListsRoute name ->
            viewLists model name

        TablesRoute name ->
            viewTables model name

        QuotesRoute name ->
            viewQuotes model name

        AsidesRoute name ->
            viewAsides model name

        NotFoundRoute ->
            notFoundView


notFoundView : Html msg
notFoundView =
    div [] [ text "Not found" ]
