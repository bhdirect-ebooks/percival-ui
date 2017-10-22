module Routing exposing (..)

import Navigation exposing (Location)
import Types exposing (..)
import UrlParser exposing (..)


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map StructRoute (s "issues" </> s "structural")
        , map FileRoute (s "issues" </> s "markup")
        , map ScripRoute (s "issues" </> s "scripture")
        , map TocRoute (s "toc")
        , map OutlineRoute (string </> s "outline")
        , map ListsRoute (string </> s "lists")
        , map TablesRoute (string </> s "tables")
        , map QuotesRoute (string </> s "quotes")
        , map AsidesRoute (string </> s "asides")
        ]


parseLocation : Location -> Route
parseLocation location =
    case parseHash matchers location of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
